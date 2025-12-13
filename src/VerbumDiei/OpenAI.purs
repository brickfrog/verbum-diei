module VerbumDiei.OpenAI
  ( LlmOutput
  , callOpenAiStructured
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)
import VerbumDiei.Artifact (Commentary, MarginalNote)

type LlmOutput =
  { marginalia :: Array MarginalNote
  , commentary :: Commentary
  }

foreign import callOpenAiStructuredImpl
  :: String
  -> String
  -> String
  -> Number
  -> (String -> Effect Unit)
  -> (LlmOutput -> Effect Unit)
  -> Effect Unit

callOpenAiStructured
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff LlmOutput
callOpenAiStructured { model, instructions, input, temperature } =
  makeAff \done -> do
    callOpenAiStructuredImpl model instructions input temperature
      (\errMsg -> done (Left (error errMsg)))
      (\output -> done (Right output))
    pure nonCanceler
