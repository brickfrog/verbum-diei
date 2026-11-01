module VerbumDiei.Http
  ( fetchText
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Data.String as String
import Promise as Promise
import Promise.Rejection as Rejection
import Web.Fetch as Fetch
import Web.Fetch.Request as Request
import Web.Fetch.Referrer as Referrer
import Web.Fetch.Response as Response

-- | Fetch a URL as text, retrying transient failures (network errors and
-- | HTTP 5xx) with linear backoff before giving up.
fetchText :: String -> Aff String
fetchText url = go 1
  where
  maxAttempts = 4

  go attempt =
    fetchTextOnce url `catchError` \err ->
      if attempt >= maxAttempts || not (retryable (message err)) then
        throwError err
      else do
        delay (Milliseconds (toNumber (attempt * 2000)))
        go (attempt + 1)

  retryable msg = not (isHttp msg) || isHttp5xx msg
  isHttp msg = String.take 5 msg == "HTTP "
  isHttp5xx msg = String.take 6 msg == "HTTP 5"

fetchTextOnce :: String -> Aff String
fetchTextOnce url = do
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
