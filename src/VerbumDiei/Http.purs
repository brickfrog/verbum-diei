module VerbumDiei.Http
  ( fetchText
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)

foreign import fetchTextImpl
  :: String
  -> (String -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

fetchText :: String -> Aff String
fetchText url =
  makeAff \done -> do
    fetchTextImpl url
      (\errMsg -> done (Left (error errMsg)))
      (\text -> done (Right text))
    pure nonCanceler

