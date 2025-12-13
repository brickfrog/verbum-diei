module VerbumDiei.OpenAI
  ( LlmOutput
  , callOpenAiStructured
  , callOpenAiExcursus
  , callOpenAiSeminaVerbi
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

type ExcursusOutput =
  { excursus :: String
  }

type SeminaVerbiOutput =
  { seminaVerbi :: String
  }

foreign import callOpenAiStructuredImpl
  :: String
  -> String
  -> String
  -> Number
  -> (String -> Effect Unit)
  -> (LlmOutput -> Effect Unit)
  -> Effect Unit

foreign import callOpenAiExcursusImpl
  :: String
  -> String
  -> String
  -> Number
  -> (String -> Effect Unit)
  -> (ExcursusOutput -> Effect Unit)
  -> Effect Unit

foreign import callOpenAiSeminaVerbiImpl
  :: String
  -> String
  -> String
  -> Number
  -> (String -> Effect Unit)
  -> (SeminaVerbiOutput -> Effect Unit)
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

callOpenAiExcursus
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff String
callOpenAiExcursus { model, instructions, input, temperature } =
  makeAff \done -> do
    callOpenAiExcursusImpl model instructions input temperature
      (\errMsg -> done (Left (error errMsg)))
      (\output -> done (Right output.excursus))
    pure nonCanceler

callOpenAiSeminaVerbi
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff String
callOpenAiSeminaVerbi { model, instructions, input, temperature } =
  makeAff \done -> do
    callOpenAiSeminaVerbiImpl model instructions input temperature
      (\errMsg -> done (Left (error errMsg)))
      (\output -> done (Right output.seminaVerbi))
    pure nonCanceler
