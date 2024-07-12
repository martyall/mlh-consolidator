module Main where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import MLHParser (parseRootFile, writeSimpleMLH)
import Node.Process (lookupEnv)

main :: Effect Unit
main = launchAff_ do
  minputFile <- liftEffect (lookupEnv "TOP_LEVEL_MLH")
  inputFile <- maybe (throwError $ error "TOP_LEVEL_MLH not set") pure minputFile
  cfg <- parseRootFile inputFile
  log $ stringify $ encodeJson cfg
  moutputFile <- liftEffect (lookupEnv "SIMPLIFIED_MLH")
  let outputFile = fromMaybe "simplified_config.mlh" moutputFile
  writeSimpleMLH outputFile cfg