module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import VerbumDiei.Bible (fetchBibleReading)

main :: Effect Unit
main = do
  launchAff_ do
    test "parses 'and' as a verse separator" do
      reading <- fetchBibleReading "John 20:1 and 2-8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses comma-separated verses" do
      reading <- fetchBibleReading "John 20:1, 2-8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses comma-separated chapters" do
      reading <- fetchBibleReading "John 20:1,21:1-2"
      assertEqual "lineRefs" [ "20:1", "21:1", "21:2" ] reading.lineRefs

    test "parses ordinal book references" do
      reading <- fetchBibleReading "1 John 1:1-4"
      assertEqual "lineRefs" [ "1", "2", "3", "4" ] reading.lineRefs

    test "resolves book aliases" do
      reading <- fetchBibleReading "Ecclesiasticus 3:2-3"
      assertEqual "reference" "Sirach 3:2-3" reading.reference
      assertEqual "lineRefs" [ "2", "3" ] reading.lineRefs

    test "resolves book abbreviations" do
      reading <- fetchBibleReading "Jn 3:16"
      assertEqual "reference" "John 3:16" reading.reference
      assertEqual "lineRefs" [ "16" ] reading.lineRefs

    test "tolerates null verses in data" do
      reading <- fetchBibleReading "Exodus 1:18"
      assertEqual "lineRefs" [ "18" ] reading.lineRefs
      assertEqual "lines length" 1 (Array.length reading.lines)

    test "parses en-dash in same-chapter range" do
      reading <- fetchBibleReading "John 20:1–8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses cross-chapter range with en-dash" do
      reading <- fetchBibleReading "1 John 2:29–3:6"
      assertEqual "lineRefs" [ "2:29", "3:1", "3:2", "3:3", "3:4", "3:5", "3:6" ] reading.lineRefs
      assertEqual "lines length" 7 (Array.length reading.lines)

    liftEffect (log "All tests passed.")

test :: String -> Aff Unit -> Aff Unit
test label action = do
  result <- attempt action
  case result of
    Left err -> throwError (error (label <> " failed: " <> show err))
    Right _ -> liftEffect (log ("ok " <> label))

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Aff Unit
assertEqual label expected actual =
  if expected == actual then
    pure unit
  else
    throwError
      (error (label <> " expected " <> show expected <> " but got " <> show actual))

expectedJohn20 :: Array String
expectedJohn20 =
  [ "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  ]
