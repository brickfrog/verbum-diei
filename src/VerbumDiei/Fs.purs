module VerbumDiei.Fs
  ( ensureDir
  , readDir
  , readTextFile
  , writeTextFile
  ) where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync as FS

ensureDir :: String -> Effect Unit
ensureDir path =
  FS.mkdir' path { recursive: true, mode: mkPerms all all all }

readDir :: String -> Effect (Array String)
readDir = FS.readdir

readTextFile :: String -> Effect String
readTextFile = FS.readTextFile UTF8

writeTextFile :: String -> String -> Effect Unit
writeTextFile path contents =
  FS.writeTextFile UTF8 path contents
