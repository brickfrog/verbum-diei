module VerbumDiei.Util
  ( nowIso
  , sha256Hex
  , splitNonEmptyLines
  ) where

import Prelude

import Data.Array (filter)
import Data.JSDate as JSDate
import Data.String (Pattern(..), split)
import Data.String.Common (trim)
import Effect (Effect)
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))

nowIso :: Effect String
nowIso = do
  now <- JSDate.now
  JSDate.toISOString now

sha256Hex :: String -> Effect String
sha256Hex input = do
  hash <- Hash.new "sha256"
  Hash.updateStr UTF8 input hash
  Hash.digestStr Hex hash

splitNonEmptyLines :: String -> Array String
splitNonEmptyLines input =
  split (Pattern "\n") input
    # map trim
    # filter (\s -> s /= "")
