module Test.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import EasyAlexa (languageModel)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert) as Assert
import Test.Unit.Main (runTest)
import Type.Prelude (Proxy(..))

main = runTest do
  suite "languageModel" do
    testTrivialSkill


data TrivialSkill
  = Launch
  | SessionEnded

derive instance genericTrivialSkill :: Generic TrivialSkill _

testTrivialSkill = do
  test "trivial skill" $
    Assert.assert
      "Valid inputs should produce a valid language model"
      (result == expected)
  where
    result = languageModel (Proxy :: Proxy TrivialSkill) "test" mempty
    expected =
      pure
        { invocationName : "test"
        , intents : []
        , types : []
        }
