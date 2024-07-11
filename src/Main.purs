module Main where

import Prelude

import Data.Array.Partial (head)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import MLHParser (parseRootFile)
import Node.Process (argv, getEnv, lookupEnv)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ lookupEnv "TOP_LEVEL_MLH"
  let fp = unsafePartial $ fromJust args
  cfg <- parseRootFile fp
  log $ show cfg