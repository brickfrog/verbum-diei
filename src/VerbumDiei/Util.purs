module VerbumDiei.Util
  ( nowIso
  , sha256Hex
  , splitNonEmptyLines
  ) where

import Prelude

import Data.Array (filter)
import Data.String (Pattern(..), split)
import Data.String.Common (trim)
import Effect (Effect)

foreign import nowIso :: Effect String

foreign import sha256Hex :: String -> Effect String

splitNonEmptyLines :: String -> Array String
splitNonEmptyLines input =
  split (Pattern "\n") input
    # map trim
    # filter (\s -> s /= "")

