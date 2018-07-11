module Test.LanguageModel (
  languageModelSuite
) where

import Prelude

import Control.Monad.Free (Free)
import Data.Generic.Rep (class Generic)
import EasyAlexa (languageModel)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert) as Assert
import Type.Prelude (Proxy(..))

data TrivialSkill
  = Launch
  | SessionEnded

derive instance genericTrivialSkill :: Generic TrivialSkill _

languageModelSuite :: Free TestF Unit
languageModelSuite = 
  suite "languageModel" do
    testTrivialSkill


testTrivialSkill :: Free TestF Unit
testTrivialSkill = 
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
