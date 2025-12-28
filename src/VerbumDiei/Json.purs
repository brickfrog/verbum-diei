module VerbumDiei.Json (stringifyPretty) where

import Data.Argonaut.Core (Json, stringifyWithIndent)

stringifyPretty :: Json -> String
stringifyPretty = stringifyWithIndent 2
