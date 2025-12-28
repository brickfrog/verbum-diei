module VerbumDiei.OpenAI
  ( LlmOutput
  , encodeLlmOutput
  , callOpenAiStructured
  , callOpenAiExcursus
  , callOpenAiSeminaVerbi
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonEmptyObject, stringify, toArray, toObject, toNumber, toString)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.Common (trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Foreign.Object as FO
import Node.Process as Process
import Promise as Promise
import Promise.Rejection as Rejection
import VerbumDiei.Artifact (Commentary, CommentNote, MarginalNote, encodeCommentary, encodeMarginalNote)
import Web.Fetch as Fetch
import Web.Fetch.Request as Request
import Web.Fetch.Response as Response

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

encodeLlmOutput :: LlmOutput -> Json
encodeLlmOutput output =
  "marginalia" := map encodeMarginalNote output.marginalia
    ~> "commentary" := encodeCommentary output.commentary
    ~> jsonEmptyObject

callOpenAiStructured
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff LlmOutput
callOpenAiStructured { model, instructions, input, temperature } = do
  let _ = temperature
  let request = buildRequest model instructions input 1200 "verbum_diei_analysis" verbumSchema
  callOpenAi request parseStructuredResponse

callOpenAiExcursus
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff String
callOpenAiExcursus { model, instructions, input, temperature } = do
  let _ = temperature
  let request = buildRequest model instructions input 2000 "verbum_diei_excursus" excursusSchema
  output <- callOpenAi request parseExcursusResponse
  pure output.excursus

callOpenAiSeminaVerbi
  :: { model :: String
     , instructions :: String
     , input :: String
     , temperature :: Number
     }
  -> Aff String
callOpenAiSeminaVerbi { model, instructions, input, temperature } = do
  let _ = temperature
  let request = buildRequest model instructions input 2000 "verbum_diei_semina_verbi" seminaVerbiSchema
  output <- callOpenAi request parseSeminaVerbiResponse
  pure output.seminaVerbi

callOpenAi :: forall a. Json -> (Json -> Either String a) -> Aff a
callOpenAi requestJson parseResponse = do
  apiKey <- liftEffect $ Process.lookupEnv "OPENAI_API_KEY"
  case apiKey of
    Nothing -> throwError (error "OPENAI_API_KEY not set")
    Just key -> do
      req <- liftEffect $ buildRequestInit key requestJson
      resp <- fetchAff req
      bodyPromise <- liftEffect $ Response.text resp
      body <- promiseToAff bodyPromise
      case jsonParser body of
        Left errMsg -> throwError (error ("Invalid JSON response: " <> errMsg))
        Right json ->
          case parseResponse json of
            Left errMsg -> throwError (error errMsg)
            Right value -> pure value

buildRequest :: String -> String -> String -> Int -> String -> Json -> Json
buildRequest model instructions input maxTokens schemaName schema =
  obj
    [ Tuple "model" (fromString model)
    , Tuple "instructions" (fromString instructions)
    , Tuple "input" (fromString input)
    , Tuple "max_output_tokens" (fromNumber (Int.toNumber maxTokens))
    , Tuple "text" (obj
        [ Tuple "verbosity" (fromString "low")
        , Tuple "format" (obj
            [ Tuple "type" (fromString "json_schema")
            , Tuple "name" (fromString schemaName)
            , Tuple "strict" (fromBoolean true)
            , Tuple "schema" schema
            ])
        ])
    ]

buildRequestInit :: String -> Json -> Effect Request.Request
buildRequestInit apiKey body = do
  let
    headers =
      { "Content-Type": "application/json"
      , "Authorization": "Bearer " <> apiKey
      }
  Request.new'
    "https://api.openai.com/v1/responses"
    { method: POST
    , headers
    , body: stringify body
    }

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

parseStructuredResponse :: Json -> Either String LlmOutput
parseStructuredResponse response = do
  case responseError response of
    Just errMsg -> Left errMsg
    Nothing ->
      case lookupField "output_parsed" response of
        Just parsed -> Right (normalizeOutput parsed)
        Nothing ->
          case extractOutputText response of
            Just raw ->
              case jsonParser raw of
                Left errMsg -> Left ("Could not parse output text: " <> errMsg)
                Right parsed -> Right (normalizeOutput parsed)
            Nothing ->
              case extractRefusal response of
                Just refusal -> Left ("Model refusal: " <> refusal)
                Nothing ->
                  let summary = summarizeResponse response
                  in Left ("No parseable output (outputTypes=" <> show summary.outputTypes <> ", contentTypes=" <> show summary.contentTypes <> ")")

parseExcursusResponse :: Json -> Either String ExcursusOutput
parseExcursusResponse response = do
  case responseError response of
    Just errMsg -> Left errMsg
    Nothing ->
      case lookupField "output_parsed" response of
        Just parsed -> Right (normalizeExcursusOutput parsed)
        Nothing ->
          case extractOutputText response of
            Just raw ->
              case jsonParser raw of
                Left errMsg -> Left ("Could not parse output text: " <> errMsg)
                Right parsed -> Right (normalizeExcursusOutput parsed)
            Nothing ->
              case extractRefusal response of
                Just refusal -> Left ("Model refusal: " <> refusal)
                Nothing ->
                  let summary = summarizeResponse response
                  in Left ("No parseable output (outputTypes=" <> show summary.outputTypes <> ", contentTypes=" <> show summary.contentTypes <> ")")

parseSeminaVerbiResponse :: Json -> Either String SeminaVerbiOutput
parseSeminaVerbiResponse response = do
  case responseError response of
    Just errMsg -> Left errMsg
    Nothing ->
      case lookupField "output_parsed" response of
        Just parsed -> Right (normalizeSeminaVerbiOutput parsed)
        Nothing ->
          case extractOutputText response of
            Just raw ->
              case jsonParser raw of
                Left errMsg -> Left ("Could not parse output text: " <> errMsg)
                Right parsed -> Right (normalizeSeminaVerbiOutput parsed)
            Nothing ->
              case extractRefusal response of
                Just refusal -> Left ("Model refusal: " <> refusal)
                Nothing ->
                  let summary = summarizeResponse response
                  in Left ("No parseable output (outputTypes=" <> show summary.outputTypes <> ", contentTypes=" <> show summary.contentTypes <> ")")

normalizeOutput :: Json -> LlmOutput
normalizeOutput parsed =
  let
    rootObj = jsonObject parsed
    marginaliaJson = fromMaybe [] (lookupArray "marginalia" rootObj)
    marginalia =
      marginaliaJson
        # Array.mapMaybe normalizeNote

    commentaryObj = jsonObject (fromMaybe jsonEmptyObject (lookupField "commentary" parsed))
    readingNotes =
      fromMaybe [] (lookupArray "reading" commentaryObj)
        # Array.mapMaybe normalizeCommentNote
    gospelNotes =
      fromMaybe [] (lookupArray "gospel" commentaryObj)
        # Array.mapMaybe normalizeCommentNote
    synthesis = normalizedString (lookupString "synthesis" commentaryObj)
    excursus = normalizedString (lookupString "excursus" commentaryObj)
  in
    { marginalia
    , commentary:
        { reading: readingNotes
        , gospel: gospelNotes
        , synthesis
        , excursus
        , seminaVerbi: ""
        }
    }

normalizeExcursusOutput :: Json -> ExcursusOutput
normalizeExcursusOutput parsed =
  let
    rootObj = jsonObject parsed
  in
    { excursus: normalizedString (lookupString "excursus" rootObj) }

normalizeSeminaVerbiOutput :: Json -> SeminaVerbiOutput
normalizeSeminaVerbiOutput parsed =
  let
    rootObj = jsonObject parsed
  in
    { seminaVerbi: normalizedString (lookupString "seminaVerbi" rootObj) }

normalizeNote :: Json -> Maybe MarginalNote
normalizeNote value = do
  noteObj <- toObject value
  let
    readingKind =
      case lookupString "readingKind" noteObj of
        Just "gospel" -> "gospel"
        _ -> "first"
    lines = normalizeLines (lookupField "lines" value)
    text = normalizedString (lookupString "text" noteObj)
  if text == "" || Array.null lines then
    Nothing
  else
    Just { readingKind, lines, text }

normalizeCommentNote :: Json -> Maybe CommentNote
normalizeCommentNote value = do
  noteObj <- toObject value
  let
    lines = normalizeLines (lookupField "lines" value)
    text = normalizedString (lookupString "text" noteObj)
  if text == "" || Array.null lines then
    Nothing
  else
    Just { lines, text }

normalizeLines :: Maybe Json -> Array Int
normalizeLines maybeJson =
  case maybeJson >>= toArray of
    Nothing -> []
    Just nums ->
      nums
        # Array.mapMaybe toInt
        # Array.filter (_ >= 1)
        # Array.nub
        # Array.sort

toInt :: Json -> Maybe Int
toInt json = do
  n <- toNumber json
  pure (Int.floor n)

normalizedString :: Maybe String -> String
normalizedString =
  case _ of
    Just s -> trim s
    Nothing -> ""

lookupField :: String -> Json -> Maybe Json
lookupField key json = do
  jsonObj <- toObject json
  FO.lookup key jsonObj

lookupString :: String -> FO.Object Json -> Maybe String
lookupString key jsonObj =
  FO.lookup key jsonObj >>= toString

lookupArray :: String -> FO.Object Json -> Maybe (Array Json)
lookupArray key jsonObj =
  FO.lookup key jsonObj >>= toArray

jsonObject :: Json -> FO.Object Json
jsonObject json =
  fromMaybe FO.empty (toObject json)

extractOutputText :: Json -> Maybe String
extractOutputText response =
  case lookupField "output_text" response >>= toString of
    Just text | text /= "" -> Just text
    _ ->
      let
        output = fromMaybe [] (lookupField "output" response >>= toArray)
        texts = output >>= outputItemText
        joined = joinWith "" texts
      in
        if joined == "" then Nothing else Just joined

extractRefusal :: Json -> Maybe String
extractRefusal response =
  let
    output = fromMaybe [] (lookupField "output" response >>= toArray)
    refusals = output >>= outputItemRefusal
  in
    Array.head refusals

outputItemText :: Json -> Array String
outputItemText item =
  case toObject item of
    Nothing -> []
    Just itemObj ->
      case lookupString "type" itemObj of
        Just "message" ->
          let
            content = fromMaybe [] (lookupArray "content" itemObj)
          in
            content >>= contentText
        _ -> []

outputItemRefusal :: Json -> Array String
outputItemRefusal item =
  case toObject item of
    Nothing -> []
    Just itemObj ->
      case lookupString "type" itemObj of
        Just "message" ->
          let
            content = fromMaybe [] (lookupArray "content" itemObj)
          in
            content >>= contentRefusal
        _ -> []

contentText :: Json -> Array String
contentText item =
  case toObject item of
    Nothing -> []
    Just contentObj ->
      case lookupString "type" contentObj of
        Just "output_text" ->
          case lookupString "text" contentObj of
            Just t -> [ t ]
            Nothing -> []
        _ -> []

contentRefusal :: Json -> Array String
contentRefusal item =
  case toObject item of
    Nothing -> []
    Just contentObj ->
      case lookupString "type" contentObj of
        Just "refusal" ->
          case lookupString "refusal" contentObj of
            Just t -> [ t ]
            Nothing -> []
        _ -> []

summarizeResponse :: Json -> { outputTypes :: Array String, contentTypes :: Array String }
summarizeResponse response =
  let
    output = fromMaybe [] (lookupField "output" response >>= toArray)
    outputTypes = output # Array.mapMaybe outputType
    contentTypes = output >>= outputContentTypes
  in
    { outputTypes, contentTypes }

outputType :: Json -> Maybe String
outputType item = do
  itemObj <- toObject item
  lookupString "type" itemObj

outputContentTypes :: Json -> Array String
outputContentTypes item =
  case toObject item of
    Nothing -> []
    Just itemObj ->
      case lookupString "type" itemObj of
        Just "message" ->
          let
            content = fromMaybe [] (lookupArray "content" itemObj)
          in
            content # Array.mapMaybe contentType
        _ -> []

contentType :: Json -> Maybe String
contentType item = do
  itemObj <- toObject item
  lookupString "type" itemObj

responseError :: Json -> Maybe String
responseError response = do
  err <- lookupField "error" response
  errObj <- toObject err
  lookupString "message" errObj

obj :: Array (Tuple String Json) -> Json
obj pairs =
  fromObject (FO.fromFoldable pairs)

verbumSchema :: Json
verbumSchema =
  obj
    [ Tuple "type" (fromString "object")
    , Tuple "additionalProperties" (fromBoolean false)
    , Tuple "properties" (obj
        [ Tuple "marginalia" (obj
            [ Tuple "type" (fromString "array")
            , Tuple "minItems" (fromNumber 6.0)
            , Tuple "maxItems" (fromNumber 18.0)
            , Tuple "items" (obj
                [ Tuple "type" (fromString "object")
                , Tuple "additionalProperties" (fromBoolean false)
                , Tuple "properties" (obj
                    [ Tuple "readingKind" (obj
                        [ Tuple "type" (fromString "string")
                        , Tuple "enum" (fromArray [ fromString "first", fromString "gospel" ])
                        ])
                    , Tuple "lines" (obj
                        [ Tuple "type" (fromString "array")
                        , Tuple "minItems" (fromNumber 1.0)
                        , Tuple "maxItems" (fromNumber 4.0)
                        , Tuple "items" (obj
                            [ Tuple "type" (fromString "integer")
                            , Tuple "minimum" (fromNumber 1.0)
                            ])
                        ])
                    , Tuple "text" (obj
                        [ Tuple "type" (fromString "string")
                        , Tuple "minLength" (fromNumber 1.0)
                        , Tuple "maxLength" (fromNumber 240.0)
                        ])
                    ])
                , Tuple "required" (fromArray [ fromString "readingKind", fromString "lines", fromString "text" ])
                ])
            ])
        , Tuple "commentary" (obj
            [ Tuple "type" (fromString "object")
            , Tuple "additionalProperties" (fromBoolean false)
            , Tuple "properties" (obj
                [ Tuple "reading" (obj
                    [ Tuple "type" (fromString "array")
                    , Tuple "minItems" (fromNumber 1.0)
                    , Tuple "maxItems" (fromNumber 4.0)
                    , Tuple "items" (commentNoteSchema 800.0)
                    ])
                , Tuple "gospel" (obj
                    [ Tuple "type" (fromString "array")
                    , Tuple "minItems" (fromNumber 1.0)
                    , Tuple "maxItems" (fromNumber 4.0)
                    , Tuple "items" (commentNoteSchema 800.0)
                    ])
                , Tuple "synthesis" (obj
                    [ Tuple "type" (fromString "string")
                    , Tuple "minLength" (fromNumber 1.0)
                    , Tuple "maxLength" (fromNumber 800.0)
                    ])
                ])
            , Tuple "required" (fromArray [ fromString "reading", fromString "gospel", fromString "synthesis" ])
            ])
        ])
    , Tuple "required" (fromArray [ fromString "marginalia", fromString "commentary" ])
    ]

commentNoteSchema :: Number -> Json
commentNoteSchema maxLength =
  obj
    [ Tuple "type" (fromString "object")
    , Tuple "additionalProperties" (fromBoolean false)
    , Tuple "properties" (obj
        [ Tuple "lines" (obj
            [ Tuple "type" (fromString "array")
            , Tuple "minItems" (fromNumber 1.0)
            , Tuple "maxItems" (fromNumber 6.0)
            , Tuple "items" (obj
                [ Tuple "type" (fromString "integer")
                , Tuple "minimum" (fromNumber 1.0)
                ])
            ])
        , Tuple "text" (obj
            [ Tuple "type" (fromString "string")
            , Tuple "minLength" (fromNumber 1.0)
            , Tuple "maxLength" (fromNumber maxLength)
            ])
        ])
    , Tuple "required" (fromArray [ fromString "lines", fromString "text" ])
    ]

excursusSchema :: Json
excursusSchema =
  obj
    [ Tuple "type" (fromString "object")
    , Tuple "additionalProperties" (fromBoolean false)
    , Tuple "properties" (obj
        [ Tuple "excursus" (obj
            [ Tuple "type" (fromString "string")
            , Tuple "minLength" (fromNumber 1.0)
            , Tuple "maxLength" (fromNumber 6000.0)
            ])
        ])
    , Tuple "required" (fromArray [ fromString "excursus" ])
    ]

seminaVerbiSchema :: Json
seminaVerbiSchema =
  obj
    [ Tuple "type" (fromString "object")
    , Tuple "additionalProperties" (fromBoolean false)
    , Tuple "properties" (obj
        [ Tuple "seminaVerbi" (obj
            [ Tuple "type" (fromString "string")
            , Tuple "minLength" (fromNumber 1.0)
            , Tuple "maxLength" (fromNumber 6000.0)
            ])
        ])
    , Tuple "required" (fromArray [ fromString "seminaVerbi" ])
    ]
