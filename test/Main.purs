module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: forall e. Effect Unit
main = do
  log "You should add some tests."
