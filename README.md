# purescript-easy-alexa

This library is a work in progress. It uses datatype generics to provide you
a handler function and a function that automatically generates the language
model for your Alexa Skill, based off a Sum Type you define.

## Usage

Write a sum type including all the intents your skill handles. Include
'Launch' and 'SessionEnded', which although they are not technically "intents",
they are events that a valid skill must handle.

If an intent supports "slots", provide a record as the argument to your
constructor.

```purescript
data Input
  = Launch
  | SessionEnded
  | IsPurescriptAmazing { isIt :: Maybe BoolSlot }
```

The types in the record must implement the `EmptyableSlot` typeclass, since
Alexa cannot guarantee that the slots you define for your intent will not
be empty. Usually the best way to do this is implement the simpler `Slot`
typeclass and take advantage of the fact that `Maybe Slot` is an instance of
`EmptyableSlot`.

```
newtype IntSlot = IntSlot Int 

instance slotIntSlot :: Slot IntSlot where
  parseSlot :: String -> Maybe IntSlot
  parseSlot s 
    | s == "yes" = Just true
    | s == "no" = Just false
    | otherwise = Nothing

  name :: String
  name = "isIt"

  slotValues :: (Proxy IntSlot) -> Array { value :: String, synonyms :: Array String }
  slotValues _ =
    [ "yes"
    , "no"
    ] <#> \s -> { value : s, synonyms: [] }
```

Here 'parseSlot' takes the string provided by alexa as the slot value, and parses it to return a member of your datatype.

'name' is a simple string that will describe this slot in the generated language model.

'slotValues' are the values for your slot that must occur in the language model. Basically, these are "hints" for Alexa to train her so that she knows what words in particular she should expect and be listening for in the context represented by your slot.

Now, your datatype Input is an instance of `AlexaInput` and you can make use of the `parseInput :: AlexaRequest -> Either InputError a` and `languageModel` functions.
```

TODO: add an actually working and complete example
