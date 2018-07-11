module Test.ParseInput (
  parseInputSuite
) where

import Prelude

import Amazon.Alexa.Types (AlexaRequest(..))
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import EasyAlexa (Builtin(..), InputError(..), parseInput)
import Foreign (Foreign)
import Simple.JSON (undefined, write)
import Test.Unit (TestF, failure, suite, test)

parseInputSuite :: Free TestF Unit
parseInputSuite = 
  suite "parseInput" do
    testParseSkill

data BuiltinNumberInput
  = SomeNumbers { n :: Builtin "AMAZON.NUMBER" Int
                , m :: Builtin "AMAZON.NUMBER" Int
                }
  | SomeNumber { n :: Builtin "AMAZON.NUMBER" Int }
  | Launch
  | SessionEnded

derive instance genericBuiltinNumberInput :: Generic BuiltinNumberInput _
instance eqBuiltinNumberInput :: Eq BuiltinNumberInput where
  eq = genericEq
instance showBuiltinNumberInput :: Show BuiltinNumberInput where
  show = genericShow

testParseSkill :: Free TestF Unit
testParseSkill = do
  test "Unrecognized Intent" do
    let expected = ((Left <<< UnknownIntent) $ "BadIntent")
        request = makeRequest { intentName : "BadIntent", slots : write {}}
        result :: Either InputError BuiltinNumberInput
        result = parseInput request

    case result of
      Left (UnknownIntent "BadIntent") → pure unit
      _ → failure "Should fail with (UnknownIntent \"BadIntent\")"

  test "Bad Slots" do
    let request = makeRequest { intentName : "SomeNumber", slots : write 2}
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Left (SlotMismatch _) → pure unit
      _ → failure "Should produce SlotMismatch"

  test "Missing Value" do
    let request = makeRequest { intentName : "SomeNumber", slots : write {} }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Missing }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Missing })"

  test "Unknown Value" do
    let request = makeRequest { intentName : "SomeNumber", slots : write { "n" : { value : "pizza" } } }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Unknown "pizza" }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Unknown \"pizza\" })"

  test "Known Value" do
    let request = makeRequest { intentName : "SomeNumber", slots : write { "n" : { value : "3" } } }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Builtin 3 }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Builtin 3 })"

  test "Known Values" do
    let slots = write { "n" : { value : "3" }, "m": { value : "pizza" } }
        request = makeRequest { intentName : "SomeNumbers", slots }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumbers { n : Builtin 3, m : Unknown "pizza" }) → pure unit
      _ → failure "Should parse to (SomeNumbers { n : Builtin 3 , m : Missing \"Pizza\"})"


  where
    makeRequest :: { intentName :: String, slots :: Foreign } → AlexaRequest
    makeRequest { intentName, slots } =
      ( IntentRequest
        { request :
          { dialogState : Nothing
          , intent : { name : intentName, confirmationStatus : Nothing, slots }
          , requestId : ""
          , timestamp : ""
          , locale : ""
          }
        , version : ""
        , session :
          { new : false
          , sessionId : ""
          , application : {}
          , attributes : undefined
          , user :
            { userId : ""
            , accessToken : Nothing
            }
          }
        , context : {}
        }
      )
