module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.Common (trim)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Node.Process as Process
import VerbumDiei.Artifact (Artifact, Commentary, Reading, ReadingKind, firstReadingKind, gospelKind)
import VerbumDiei.BibleApi (fetchBibleApiReading)
import VerbumDiei.Fs (ensureDir, readDir, writeTextFile)
import VerbumDiei.Http (fetchText)
import VerbumDiei.Json (stringifyPretty)
import VerbumDiei.Observances (getObservances)
import VerbumDiei.OpenAI (callOpenAiStructured)
import VerbumDiei.Rss (FeedItem, parseWordOfDayFeed)
import VerbumDiei.Site (renderArchivePage, renderArtifactPage)
import VerbumDiei.Util (nowIso, sha256Hex)

main :: Effect Unit
main = do
  launchAff_ run

rssUrl :: String
rssUrl = "https://www.vaticannews.va/en/word-of-the-day.rss.xml"

run :: Aff Unit
run = do
  args <- liftEffect Process.argv
  let targetDate = argValue "--date" args

  log "Fetching Vatican News RSS…"
  rssXml <- fetchText rssUrl
  let feed = parseWordOfDayFeed rssXml

  item <- case targetDate of
    Nothing ->
      case Array.head feed.items of
        Nothing -> throwError (error "RSS feed had no items")
        Just it -> pure it
    Just d ->
      case Array.find (\it -> it.date == d) feed.items of
        Nothing -> throwError (error ("No RSS item matched date " <> d))
        Just it -> pure it

  readings <- fetchReadings item
  observances <- liftEffect $ getObservances item.date

  generatedAt <- liftEffect nowIso

  openAiKeyRaw <- liftEffect $ Process.lookupEnv "OPENAI_API_KEY"
  let openAiKey = openAiKeyRaw >>= \k -> if trim k == "" then Nothing else Just k
  model <- liftEffect $ fromMaybe "gpt-5.2" <$> preferredModel

  { marginalia, commentary, calls } <- case openAiKey of
    Nothing -> do
      log "OPENAI_API_KEY not set; skipping LLM generation."
      pure
        { marginalia: []
        , commentary: emptyCommentary
        , calls: []
        }
    Just _ -> do
      log "Generating marginalia + commentary (structured)…"
      let input = renderPromptInput readings
      let instructions = llmInstructions

      result <-
        attempt $
          callOpenAiStructured
            { model
            , instructions
            , input
            , temperature: 0.2
            }

      case result of
        Left e -> do
          log ("OpenAI generation failed; continuing without LLM output. " <> show e)
          pure
            { marginalia: []
            , commentary: emptyCommentary
            , calls: []
            }
        Right llmOutput -> do
          let sanitized = sanitizeLlmOutput readings llmOutput

          llmInputSha <- liftEffect $ sha256Hex (instructions <> "\n\n" <> input)
          llmOutputSha <- liftEffect $ sha256Hex (stringifyPretty sanitized)

          pure
            { marginalia: sanitized.marginalia
            , commentary: sanitized.commentary
            , calls:
                [ { name: "analysis"
                  , model
                  , inputSha256: llmInputSha
                  , outputSha256: llmOutputSha
                  }
                ]
            }

  let artifact =
        { date: item.date
        , source:
            { rssUrl
            , itemUrl: item.guid
            , title: item.title
            , guid: item.guid
            }
        , observances
        , readings
        , marginalia
        , commentary
        , llm:
            { generatedAt
            , calls
            }
        }

  writeOutputs artifact

fetchReadings :: FeedItem -> Aff (Array Reading)
fetchReadings item = do
  item.readings # traverse \r -> do
    api <- fetchBibleApiReading r.bibleApiReference
    pure
      { kind: readingKindFromString r.kind
      , heading: r.heading
      , reference: api.reference
      , bibleApiReference: r.bibleApiReference
      , translation: api.translation
      , lines: api.lines
      }

readingKindFromString :: String -> ReadingKind
readingKindFromString = case _ of
  "gospel" -> gospelKind
  _ -> firstReadingKind

preferredModel :: Effect (Maybe String)
preferredModel = do
  explicit <- Process.lookupEnv "VERBUM_OPENAI_MODEL"
  case explicit of
    Just m -> pure (Just m)
    Nothing -> Process.lookupEnv "OPENAI_MODEL"

renderPromptInput :: Array Reading -> String
renderPromptInput readings =
  String.joinWith "\n\n" $
    readings <#> \r ->
      let
        header =
          case r.kind of
            k | k == gospelKind -> "[GOSPEL]"
            _ -> "[READING]"
        numbered =
          r.lines
            # Array.mapWithIndex \i line -> String.joinWith "" [ show (i + 1), ". ", line ]
      in
        String.joinWith "\n"
          [ header
          , r.heading
          , r.reference
          , String.joinWith "\n" numbered
          ]

llmInstructions :: String
llmInstructions =
  String.joinWith "\n"
    [ "You write marginalia on scripture. Cold, lapidary, non-pastoral. No emojis. No apologies."
    , "You must return JSON that matches the provided schema."
    , "All line numbers must refer to the numbered lines inside the corresponding [READING] or [GOSPEL] block."
    , "Do not quote long spans of scripture; only short fragments if necessary."
    ]

emptyCommentary :: Commentary
emptyCommentary =
  { reading: []
  , gospel: []
  , synthesis: ""
  }

sanitizeLlmOutput
  :: Array Reading
  -> { marginalia :: Array { readingKind :: ReadingKind, lines :: Array Int, text :: String }
     , commentary :: Commentary
     }
  -> { marginalia :: Array { readingKind :: ReadingKind, lines :: Array Int, text :: String }
     , commentary :: Commentary
     }
sanitizeLlmOutput readings llmOutput =
  let
    maxLinesFor kind =
      case Array.find (\r -> r.kind == kind) readings of
        Nothing -> 0
        Just r -> Array.length r.lines

    normalizeLineRefs max =
      Array.filter (\n -> n >= 1 && n <= max)
        >>> Array.nub
        >>> Array.sort

    fixMarginalNote note =
      let
        max = maxLinesFor note.readingKind
        refs = normalizeLineRefs max note.lines
      in
        note { lines = refs, text = trim note.text }

    marginalia =
      llmOutput.marginalia
        <#> fixMarginalNote
        # Array.filter (\n -> n.text /= "" && Array.length n.lines > 0)

    firstMax = maxLinesFor firstReadingKind
    gospelMax = maxLinesFor gospelKind

    fixCommentNote max note =
      note { lines = normalizeLineRefs max note.lines, text = trim note.text }

    commentary =
      { reading:
          llmOutput.commentary.reading
            <#> fixCommentNote firstMax
            # Array.filter (\n -> n.text /= "" && Array.length n.lines > 0)
      , gospel:
          llmOutput.commentary.gospel
            <#> fixCommentNote gospelMax
            # Array.filter (\n -> n.text /= "" && Array.length n.lines > 0)
      , synthesis: trim llmOutput.commentary.synthesis
      }
  in
    { marginalia, commentary }

isAsciiDigit :: Char -> Boolean
isAsciiDigit c =
  let
    code = Char.toCharCode c
  in
    code >= 48 && code <= 57

allDigits :: String -> Boolean
allDigits =
  CodeUnits.toCharArray >>> Array.all isAsciiDigit

isIsoDate :: String -> Boolean
isIsoDate s =
  case split (Pattern "-") s of
    [ y, m, d ] ->
      CodeUnits.length y == 4
        && CodeUnits.length m == 2
        && CodeUnits.length d == 2
        && allDigits y
        && allDigits m
        && allDigits d
    _ -> false

extractDateFromDataFilename :: String -> Maybe String
extractDateFromDataFilename filename =
  case CodeUnits.stripSuffix (Pattern ".json") filename of
    Just base | isIsoDate base -> Just base
    _ -> Nothing

listDataDates :: Effect (Array String)
listDataDates = do
  entries <- readDir "data"
  pure $
    entries
      # Array.mapMaybe extractDateFromDataFilename
      # Array.sort
      # Array.reverse

writeOutputs :: Artifact -> Aff Unit
writeOutputs artifact = do
  let json = stringifyPretty artifact
  let rootHtml =
        renderArtifactPage
          { assetPrefix: ""
          , homeHref: ""
          , archiveHref: "archive/"
          , permalinkHref: "d/" <> artifact.date <> "/"
          }
          artifact

  let dayHtml =
        renderArtifactPage
          { assetPrefix: "../../"
          , homeHref: "../../"
          , archiveHref: "../../archive/"
          , permalinkHref: ""
          }
          artifact

  liftEffect do
    ensureDir "data"
    ensureDir "public"
    ensureDir "public/data"
    ensureDir "public/d"
    ensureDir ("public/d/" <> artifact.date)
    ensureDir "public/archive"

    writeTextFile ("data/" <> artifact.date <> ".json") json
    writeTextFile ("public/data/" <> artifact.date <> ".json") json
    writeTextFile "public/index.html" rootHtml
    writeTextFile ("public/d/" <> artifact.date <> "/index.html") dayHtml

  dates <- liftEffect listDataDates
  let archiveHtml =
        renderArchivePage
          { assetPrefix: "../"
          , homeHref: "../"
          , dayHrefPrefix: "../d/"
          }
          dates

  liftEffect do
    writeTextFile "public/archive/index.html" archiveHtml

  log ("Wrote public/index.html, public/d/" <> artifact.date <> "/index.html, and public/archive/index.html")

argValue :: String -> Array String -> Maybe String
argValue key argv =
  case Array.findIndex (_ == key) argv of
    Just i -> Array.index argv (i + 1)
    Nothing -> findEquals key argv

findEquals :: String -> Array String -> Maybe String
findEquals key argv =
  argv
    # Array.findMap \arg -> case split (Pattern "=") arg of
        [ k, v ] | k == key -> Just v
        _ -> Nothing
