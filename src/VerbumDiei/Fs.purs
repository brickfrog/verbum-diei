module VerbumDiei.Fs
  ( ensureDir
  , readDir
  , writeTextFile
  ) where

import Prelude

import Effect (Effect)

foreign import ensureDir :: String -> Effect Unit

foreign import readDir :: String -> Effect (Array String)

foreign import writeTextFile :: String -> String -> Effect Unit
