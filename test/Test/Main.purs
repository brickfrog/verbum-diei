module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
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
