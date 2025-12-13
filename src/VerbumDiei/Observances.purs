module VerbumDiei.Observances
  ( getObservances
  ) where

import Effect (Effect)

import VerbumDiei.Artifact (Observances)

foreign import getObservances :: String -> Effect Observances
