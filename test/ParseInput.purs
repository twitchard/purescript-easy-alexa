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
import EasyAlexa (Builtin(..), InputError(..), parseInput)
import Foreign (Foreign)
import Simple.JSON (undefined, write)
import Test.Unit (TestF, failure, suite, test, testOnly)

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

  testOnly "Launch Request" do
    let request = launchRequest
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right Launch → pure unit
      _ → failure "Should produce Launch"

  test "Unrecognized Intent" do
    let request = makeIntentRequest { intentName : "BadIntent", slots : write {}}
        result :: Either InputError BuiltinNumberInput
        result = parseInput request

    case result of
      Left (UnknownIntent "BadIntent") → pure unit
      _ → failure "Should fail with (UnknownIntent \"BadIntent\")"

  test "Bad Slots" do
    let request = makeIntentRequest { intentName : "SomeNumberIntent", slots : write 2}
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Left (SlotMismatch _) → pure unit
      _ → failure "Should produce SlotMismatch"

  test "Missing Slot" do
    let request = makeIntentRequest { intentName : "SomeNumberIntent", slots : write {} }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Missing }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Missing })"

  test "Missing Value Inside Slot" do
    let request = makeIntentRequest { intentName : "SomeNumberIntent", slots : write { "n" : {} } }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Missing }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Missing })"

  test "Unknown Value" do
    let request = makeIntentRequest { intentName : "SomeNumberIntent", slots : write { "n" : { value : "pizza" } } }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Unknown "pizza" }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Unknown \"pizza\" })"

  test "Known Value" do
    let request = makeIntentRequest { intentName : "SomeNumberIntent", slots : write { "n" : { value : "3" } } }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumber { n : Builtin 3 }) → pure unit
      _ → failure "Should parse to (SomeNumber { n : Builtin 3 })"

  test "Known Values" do
    let slots = write { "n" : { value : "3" }, "m": { value : "pizza" } }
        request = makeIntentRequest { intentName : "SomeNumbersIntent", slots }
        result :: Either InputError BuiltinNumberInput
        result = parseInput request
    case result of
      Right (SomeNumbers { n : Builtin 3, m : Unknown "pizza" }) → pure unit
      _ → failure "Should parse to (SomeNumbers { n : Builtin 3 , m : Missing \"Pizza\"})"


  where
    launchRequest :: AlexaRequest
    launchRequest =
      LaunchRequest
        { request : { requestId : ""
                    , timestamp : ""
                    , locale : ""
                    }
        , version : ""
        , session : { new : true
                    , sessionId : ""
                    , application : {}
                    , attributes : undefined
                    , user : emptyUser
                    }
        , context : {}
        }

    makeIntentRequest :: { intentName :: String, slots :: Foreign } → AlexaRequest
    makeIntentRequest { intentName, slots } =
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
          , user : emptyUser
          }
        , context : {}
        }
      )
    emptyUser = { userId : ""
                , accessToken : Nothing
                }
