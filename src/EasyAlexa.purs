module EasyAlexa where

import Prelude

import Amazon.Alexa.LanguageModel (LanguageModel)
import Amazon.Alexa.Types (AlexaRequest(..))
import Control.Monad.Error.Class (throwError)
import Data.Array (elem, filter, find, fromFoldable, length, nub, null, partition)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (filter) as NE
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.List (List(..), reverse, (:))
import Data.List (fromFoldable) as List
import Data.Map (Map)
import Data.Map (keys, lookup, member) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (for_)
import Foreign (Foreign)
import Foreign.Object (Object) as Object
import Foreign.Object (lookup)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (insert)
import Simple.JSON (read)
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..), reflectSymbol)

class EmptyableSlot a where
  parseSlot' :: String → a
  empty :: a
  name' :: Proxy a → String
  slotValues' :: (Proxy a) → Array { value :: String, synonyms :: Array String }

class Slot a where
  parseSlot :: String → Maybe a
  name :: Proxy a → String
  slotValues :: (Proxy a) → Array { value :: String, synonyms :: Array String }

instance maybeEmptyableSlot ::
  ( Slot a ) => EmptyableSlot (Maybe a) where
  name' _ = name (Proxy :: Proxy a)
  slotValues' _ = slotValues (Proxy :: Proxy a)
  parseSlot' = parseSlot
  empty = Nothing

data InputError
  = UnknownIntent String
  | SlotMismatch Foreign

type SlotRec =
  { name :: String
  , typeName :: String
  , values :: Array { value :: String, synonyms :: Array String}
  }

type InputRec =
  { inputName :: String
  , slotRecs :: Array SlotRec
  }

class AlexaInput a where
  parseInput :: AlexaRequest → Either InputError a
  inputList
    :: (Proxy a)
    → NonEmptyArray InputRec

instance alexaInput ::
  ( Generic a rep
  , AlexaInputRep rep
  ) => AlexaInput a where
  parseInput = map to <<< parseInput'
  inputList _ = inputList' (Proxy :: Proxy rep)

class AlexaInputRep rep where
  parseInput' :: AlexaRequest → Either InputError rep
  inputList'
    :: (Proxy rep)
    → NonEmptyArray InputRec

instance sAlexaInputRep ::
  ( AlexaInputRep (Constructor aname a)
  , AlexaInputRep b
  , IsSymbol aname
  )
  => AlexaInputRep (Sum (Constructor aname a) b) where
    parseInput' ar =
      if nameMatches (reflectSymbol (SProxy :: SProxy aname))
         then Inl <$> parsea ar
         else Inr <$> parseb ar
      where
        nameMatches name = case ar of
          LaunchRequest _ → name == "Launch"
          SessionEndedRequest _ → name == "SessionEnded"
          IntentRequest { request: { intent : { "name": name' } } } → name == name' <> "Intent"
        parsea :: AlexaRequest → Either InputError (Constructor aname a)
        parsea = parseInput'

        parseb :: AlexaRequest → Either InputError b
        parseb = parseInput'

    inputList' _ =
      inputList' (Proxy :: Proxy (Constructor aname a)) <>
      inputList' (Proxy :: Proxy b)

instance zAlexaInputRep ::
  ( IsSymbol aname
  ) => AlexaInputRep (Constructor aname NoArguments) where
  parseInput' ar = parseInput''
    where
      cname = reflectSymbol (SProxy :: SProxy aname)
      parseInput''
        | cname == "Launch" = case ar of
            LaunchRequest _ → pure $ Constructor NoArguments
            _ → throwError $ UnknownIntent ""
        | cname == "SessionEnded" = case ar of
            SessionEndedRequest _ → pure $ Constructor NoArguments
            _ → throwError $ UnknownIntent ""
        | otherwise = case ar of
            IntentRequest { request : { intent : {name, slots} } } →
              if (name == cname <> "Intent")
              then pure (Constructor NoArguments)
              else throwError $ UnknownIntent name
            _ → throwError $ UnknownIntent ""

  inputList' _ = pure
    { inputName : reflectSymbol (SProxy :: SProxy aname)
    , slotRecs : mempty
    }

instance zAlexaInputRep' ::
  ( RowToList row rs
  , AlexaInputRow rs row
  , IsSymbol cname
  ) => AlexaInputRep (Constructor cname (Argument (Record row))) where
  parseInput' ar = Constructor <$>
    Argument <$> case ar of
      IntentRequest { request : { intent : {name, slots} } } → do
        unless
          (name == reflectSymbol (SProxy :: SProxy cname) <> "Intent") $
          throwError $ UnknownIntent name
        case read slots of
          Left _ → throwError $ SlotMismatch slots
          Right m → parseSlots (RLProxy :: RLProxy rs) m
      SessionEndedRequest _ → throwError $ UnknownIntent "SessionEnded"
      LaunchRequest _ → throwError $ UnknownIntent "Launch"

  inputList' _ = pure
    { inputName : reflectSymbol (SProxy :: SProxy cname)
    , slotRecs : slotList (RLProxy :: RLProxy rs)
    }


class AlexaInputRow (rs :: RowList) (r :: # Type) | rs → r where
  parseSlots
    :: RLProxy rs
    → Object.Object { value :: Maybe String }
    → Either InputError (Record r)

  slotList
    :: RLProxy rs
    → Array SlotRec

instance consAlexaInputRow ::
  ( Lacks sym row'
  , IsSymbol sym
  , RowToList row rl
  , RowToList row' rl'
  , Cons sym ty row' row
  , EmptyableSlot ty
  , AlexaInputRow rl' row'
  ) => AlexaInputRow (Cons sym ty rl') row
  where
    parseSlots _ m =
      case lookup (reflectSymbol name) m of
         Nothing → map (insert name empty) rest
         Just val → case val.value of
            Just v → map (insert name (parseSlot' v)) rest
            Nothing → map (insert name empty) rest

      where
        rest = parseSlots (RLProxy :: RLProxy rl') m
        name = (SProxy :: SProxy sym)

    slotList _ = pure
      { name : reflectSymbol (SProxy :: SProxy sym)
      , typeName : name' (Proxy :: Proxy ty)
      , values : slotValues' (Proxy :: Proxy ty)
      }

instance nilAlexaInputRow :: AlexaInputRow Nil () where
  parseSlots _ m = pure {}
  slotList _ = mempty

languageModel :: ∀ a
  . AlexaInput a
  => Proxy a
  → String
  → Map String (NonEmptyArray String)
  → Either String LanguageModel
languageModel _ invocationName samples = do
  --builtinIntentsCantHaveSlots
  customIntentsMustHaveSamples
  noExtraIntentSamples
  noExtraSlotNames
  mustHandleSessionEnded -- TODO: This check can move to compile time
  mustHandleLaunch -- TODO: This check can move to compile time
  pure $ { invocationName
         , intents
         , types
         }
  where
    intents = renderIntent <$> allIntentRecs # fromFoldable
    types = renderSlotType <$> allSlotRecs # (filter \{values} → length values > 0)

    mustHandleSessionEnded =
      intentList'
        # find (\x → x.inputName == "SessionEnded")
        # note "Input type must contain SessionEnded constructor to handle SessionEndedRequests"
        >>= (\x → failUnless (null x.slotRecs) "SessionEnded constructor should not have a slot")

    mustHandleLaunch =
      intentList'
        # find (\x → x.inputName == "Launch")
        # note "Input type must contain Launch constructor to handle LaunchIntents"
        >>= (\x → failUnless (null x.slotRecs) "Launch constructor should not have a slot")

    intentList' :: NonEmptyArray InputRec
    intentList' = inputList (Proxy :: Proxy a)

    allIntentRecs :: Array InputRec
    allIntentRecs = intentList'
      # NE.filter (\x → x.inputName /= "SessionEnded")
      # filter (\x → x.inputName /= "Launch")

    allSlotRecs :: Array SlotRec
    allSlotRecs = (join <<< fromFoldable) $ allIntentRecs <#> (\i → i.slotRecs)

    renderIntent i = { name , slots , samples : samples' }
      where
        name =
          if elem i.inputName standardBuiltins
            then "AMAZON." <> i.inputName <> "Intent"
            else i.inputName <> "Intent"
        slots = i.slotRecs <#> \{name: slotName, typeName} → { name: slotName, type: typeName}
        samples' = (Map.lookup i.inputName samples) <#> fromFoldable # fromMaybe []

    renderSlotType sr = { name, values }
      where
        name = sr.typeName
        values = sr.values <#> \v →
            { name: { value : v.value, synonyms: v.synonyms } }

    partitionedIntentRecs =
      partition (\i → elem i.inputName standardBuiltins) (fromFoldable allIntentRecs)
    builtinIntentRecs = partitionedIntentRecs.yes
    customIntentRecs = partitionedIntentRecs.no

    failUnless :: Boolean → String → Either String Unit
    failUnless cond msg = when (not cond) (Left msg)

    builtinIntentsCantHaveSlots =
      for_ builtinIntentRecs \i →
        failUnless
          (null i.slotRecs)
          ("built-in intent " <> i.inputName <> " should not have a slot")

    customIntentsMustHaveSamples =
      for_ customIntentRecs \i →
        failUnless
          (Map.member i.inputName samples)
          ("custom intent " <> i.inputName <> " must have sample utterances")

    noExtraIntentSamples =
      for_ (Map.keys samples) \name →
        failUnless
          (elem name usedIntentNames)
          ("sample utterances were provided for an intent called " <> name <>
           ", which did not correspond to a constructor in the input type"
          )
      where
            usedIntentNames = customIntentRecs <#> \i → i.inputName

    noExtraSlotNames =
      for_ customIntentRecs \i → do
        utterances <- Map.lookup i.inputName samples
                        # note ("custom intent " <> i.inputName <>
                                " must have sample utterances")
        let usedInUtterances = (nub <<< join <<< fromFoldable) $ (parseSlots <$> utterances)
        let usedInIntentDefinition = i.slotRecs <#> \s → s.name
        for_ usedInUtterances \name → do
          failUnless
            (elem name usedInIntentDefinition)
            ("A slot placeholder {" <> name <>
             "} was inside a sample utterance for the intent " <> i.inputName <>
             " but was not a field name on the intent's associated record"
            )
        for_ usedInIntentDefinition \name → do
          failUnless
            (elem name usedInUtterances)
            ("A slot named " <> name <>
             " was a field name on the record associated with the intent " <> i.inputName <>
              " but none of the sample utterances for that intent contained" <>
                " a slot placeholder {" <> name <> "}"
            )

    parseSlots :: String → Array String
    parseSlots utterance =
      (fromFoldable <<< reverse) $
        step Nil Nothing      chars
      where
        step acc _            Nil        = acc
        step acc Nothing      ('{' : xs) = step acc        (Just Nil      ) xs
        step acc Nothing      ( x  : xs) = step acc        Nothing          xs
        step acc (Just inner) ('}' : xs) = step (word:acc) Nothing          xs
          where word = (fromCharArray <<< fromFoldable <<< reverse) $ inner
        step acc (Just inner) ( x  : xs) = step acc        (Just (x:inner)) xs

        chars = (List.fromFoldable (toCharArray utterance))

    allSlotNames = inputList

standardBuiltins :: Array String
standardBuiltins =
  [ "Cancel"
  , "Fallback"
  , "Help"
  , "LoopOff"
  , "LoopOn"
  , "Next"
  , "No"
  , "Pause"
  , "Previous"
  , "Repeat"
  , "Resume"
  , "ShuffleOff"
  , "ShuffleOn"
  , "StartOver"
  , "Stop"
  , "Yes"
  ]

data Builtin (sym :: Symbol) a
  = Builtin a
  | Unknown String
  | Missing

derive instance genericBuiltin :: Generic (Builtin sym a) _
instance eqBuiltin ::
  ( Eq a
  ) => Eq (Builtin sym a) where
  eq = genericEq
instance showBuiltin ::
  ( Show a
  ) => Show (Builtin sym a) where
  show = genericShow

exactly :: ∀ sym a. Builtin sym a → Maybe a
exactly (Builtin a) = Just a
exactly (Unknown s) = Nothing
exactly Missing     = Nothing

class BuiltinSlot (sym :: Symbol) a where
  parseBuiltin :: (SProxy sym) → String → Maybe a

instance slotBuiltinSlot ::
  ( BuiltinSlot sym a
  , IsSymbol sym
  ) => EmptyableSlot (Builtin sym a) where
  parseSlot' s =
    case (parseBuiltin (SProxy :: SProxy sym) s) of
      Nothing → (Unknown s)
      Just b → (Builtin b)
  name' _ = reflectSymbol (SProxy :: SProxy sym)
  slotValues' = mempty
  empty = Missing

instance builtinSlotAmazonNumber :: BuiltinSlot "AMAZON.NUMBER" Int where
  parseBuiltin _ = Int.fromString
