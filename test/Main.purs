module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Generic.Rep (class Generic)
import EasyAlexa (languageModel)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert) as Assert
import Test.Unit.Main (runTest)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = runTest do
  suite "languageModel" do
    testTrivialSkill

data TrivialSkill
  = Launch
  | SessionEnded

derive instance genericTrivialSkill :: Generic TrivialSkill _

testTrivialSkill :: Free TestF Unit
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
