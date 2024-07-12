module Main where

import Prelude

import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import MLHParser (parseRootFile, writeSimpleMLH)
import Node.Process (lookupEnv)

main :: Effect Unit
main = launchAff_ do
  minputFile <- liftEffect (lookupEnv "TOP_LEVEL_MLH")
  inputFile <- maybe (throwError $ error "TOP_LEVEL_MLH not set") pure minputFile
  cfg <- parseRootFile inputFile
  moutputFile <- liftEffect (lookupEnv "SIMPLIFIED_MLH")
  let outputFile = fromMaybe inputFile moutputFile
  writeSimpleMLH outputFile cfg