module VerbumDiei.Breviarium
  ( OfficeOption
  , OfficePayload
  , getOfficePayload
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Promise as Promise
import Promise.Rejection as Rejection

type OfficeOption =
  { id :: String
  , cycle :: String
  , readingRef :: String
  , reading :: String
  , finalPrayer :: String
  }

type OfficePayload =
  { officium :: Array OfficeOption
  , laudes :: Array OfficeOption
  , tertia :: Array OfficeOption
  , sexta :: Array OfficeOption
  , nona :: Array OfficeOption
  , vesperae :: Array OfficeOption
  , completorium :: Array OfficeOption
  }

foreign import getOfficePayloadPromise :: String -> Effect (Promise.Promise OfficePayload)

getOfficePayload :: String -> Aff OfficePayload
getOfficePayload dateIso = do
  promise <- liftEffect $ getOfficePayloadPromise dateIso
  promiseToAff promise

promiseToAff :: forall a. Promise.Promise a -> Aff a
promiseToAff promise =
  makeAff \done -> do
    void $ Promise.thenOrCatch
      (\value -> do
        done (Right value)
        pure (Promise.resolve unit)
      )
      (\rejection -> do
        done (Left (error (rejectionMessage rejection)))
        pure (Promise.resolve unit)
      )
      promise
    pure nonCanceler

rejectionMessage :: Rejection.Rejection -> String
rejectionMessage rejection =
  case Rejection.toError rejection of
    Just err -> message err
    Nothing -> "Promise rejected"
