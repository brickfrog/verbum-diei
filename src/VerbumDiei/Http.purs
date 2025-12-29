module VerbumDiei.Http
  ( fetchText
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Promise as Promise
import Promise.Rejection as Rejection
import Web.Fetch as Fetch
import Web.Fetch.Request as Request
import Web.Fetch.Referrer as Referrer
import Web.Fetch.Response as Response

fetchText :: String -> Aff String
fetchText url = do
  req <- liftEffect $ Request.new' url { referrer: Referrer.ReferrerUrl "" }
  resp <- fetchAff req
  if Response.ok resp then do
    bodyPromise <- liftEffect $ Response.text resp
    promiseToAff bodyPromise
  else do
    let statusLine = show (Response.status resp) <> " " <> Response.statusText resp
    throwError (error ("HTTP " <> statusLine))

fetchAff :: Request.Request -> Aff Response.Response
fetchAff req = do
  promise <- liftEffect $ Fetch.fetch req
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
