module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

square :: Int -> Int 
square x = x * x

main :: Effect Unit
main = do
  log "🍝"
