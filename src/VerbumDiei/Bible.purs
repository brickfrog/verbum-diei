module VerbumDiei.Bible
  ( BibleReading
  , fetchBibleReading
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Array as Array
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)
import VerbumDiei.Artifact (Translation)
import VerbumDiei.Bible.Citation (expandCitation, parseCitation, parseReference)

type BibleReading =
  { reference :: String
  , translation :: Translation
  , lineRefs :: Array String
  , lines :: Array String
  }

foreign import fetchBibleReadingImpl
  :: String
  -> String
  -> Array { chapter :: Int, verse :: Int }
  -> (String -> Effect Unit)
  -> (BibleReading -> Effect Unit)
  -> Effect Unit

fetchBibleReading :: String -> Aff BibleReading
fetchBibleReading reference =
  makeAff \done -> do
    case parseReference reference of
      Left errMsg -> do
        done (Left (error errMsg))
        pure nonCanceler
      Right { book, citation } ->
        case parseCitation citation of
          Left errMsg -> do
            done (Left (error errMsg))
            pure nonCanceler
          Right segments -> do
            let refs = expandCitation segments
            if Array.null refs then do
              done (Left (error ("Could not parse citation: " <> citation)))
              pure nonCanceler
            else do
              fetchBibleReadingImpl book citation refs
                (\errMsg -> done (Left (error errMsg)))
                (\result -> done (Right result))
              pure nonCanceler
