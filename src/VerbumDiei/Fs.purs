module VerbumDiei.Fs
  ( ensureDir
  , readDir
  , writeTextFile
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync as FS

ensureDir :: String -> Effect Unit
ensureDir path =
  FS.mkdir' path { recursive: true, mode: mkPerms all all all }

readDir :: String -> Effect (Array String)
readDir path = do
  result <- try (FS.readdir path)
  case result of
    Left _ -> pure []
    Right entries -> pure entries

writeTextFile :: String -> String -> Effect Unit
writeTextFile path contents =
  FS.writeTextFile UTF8 path contents
