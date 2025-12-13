module VerbumDiei.BibleApi
  ( BibleApiReading
  , fetchBibleApiReading
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)
import VerbumDiei.Artifact (Translation)

type BibleApiReading =
  { reference :: String
  , translation :: Translation
  , lines :: Array String
  }

foreign import fetchBibleApiReadingImpl
  :: String
  -> (String -> Effect Unit)
  -> (BibleApiReading -> Effect Unit)
  -> Effect Unit

fetchBibleApiReading :: String -> Aff BibleApiReading
fetchBibleApiReading bibleApiReference =
  makeAff \done -> do
    fetchBibleApiReadingImpl bibleApiReference
      (\errMsg -> done (Left (error errMsg)))
      (\result -> done (Right result))
    pure nonCanceler

