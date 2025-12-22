module VerbumDiei.Bible
  ( BibleReading
  , fetchBibleReading
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)
import VerbumDiei.Artifact (Translation)

type BibleReading =
  { reference :: String
  , translation :: Translation
  , lineRefs :: Array String
  , lines :: Array String
  }

foreign import fetchBibleReadingImpl
  :: String
  -> (String -> Effect Unit)
  -> (BibleReading -> Effect Unit)
  -> Effect Unit

fetchBibleReading :: String -> Aff BibleReading
fetchBibleReading reference =
  makeAff \done -> do
    fetchBibleReadingImpl reference
      (\errMsg -> done (Left (error errMsg)))
      (\result -> done (Right result))
    pure nonCanceler
