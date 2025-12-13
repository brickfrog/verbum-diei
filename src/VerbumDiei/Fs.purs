module VerbumDiei.Fs
  ( ensureDir
  , writeTextFile
  ) where

import Prelude

import Effect (Effect)

foreign import ensureDir :: String -> Effect Unit

foreign import writeTextFile :: String -> String -> Effect Unit
