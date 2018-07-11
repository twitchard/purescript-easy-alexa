module Test.Main where

import Prelude

import Effect (Effect)
import Test.LanguageModel (languageModelSuite)
import Test.ParseInput (parseInputSuite)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  languageModelSuite
  parseInputSuite

