module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
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
import VerbumDiei.Artifact (Artifact, Commentary, HourEntry, Reading, ReadingKind, encodeArtifact, firstReadingKind, gospelKind)
import VerbumDiei.Bible (fetchBibleReading)
import VerbumDiei.Breviarium (OfficeOption, OfficePayload, getOfficePayload)
import VerbumDiei.Fs (ensureDir, readDir, writeTextFile)
import VerbumDiei.Http (fetchText)
import VerbumDiei.Json (stringifyPretty)
import VerbumDiei.Observances (getObservances)
import VerbumDiei.OpenAI (callOpenAiExcursus, callOpenAiSeminaVerbi, callOpenAiStructured, callOpenAiTranslation, encodeLlmOutput)
import VerbumDiei.Prompts (heterodoxPrompt, hoursTranslationPrompt, llmInstructions, seminaVerbiPrompt)
import VerbumDiei.Rss (FeedItem, parseWordOfDayFeed)
import VerbumDiei.Site (renderAppShellPage, renderArchivePage, renderArtifactPage)
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
  let preflightOnly = hasFlag "--preflight" args || hasFlag "--check" args

  log "Fetching Vatican News RSS…"
  rssXml <- fetchText rssUrl
  let feed = parseWordOfDayFeed rssXml

  if preflightOnly then do
    preflightFeed feed.items
  else do
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
    translateHours <- liftEffect shouldTranslateHours
    let hoursTranslationKey = if translateHours then openAiKey else Nothing
    hoursOfPrayer <- buildHoursOfPrayer item.date model hoursTranslationKey

    { marginalia, commentary, calls } <- case openAiKey of
      Nothing -> do
        log "OPENAI_API_KEY not set; skipping LLM generation."
        pure
          { marginalia: []
          , commentary: emptyCommentary
          , calls: []
          }
      Just _ -> do
        let input = renderPromptInput readings
        log "Generating marginalia + commentary (structured)…"

        analysisResult <-
          attempt $
            callOpenAiStructured
              { model
              , instructions: llmInstructions
              , input
              , temperature: 0.2
              }

        base <- case analysisResult of
          Left e -> do
            log ("OpenAI analysis failed; continuing without marginalia/commentary. " <> show e)
            pure
              { marginalia: []
              , commentary: emptyCommentary
              , calls: []
              }
          Right llmOutput -> do
            let sanitized = sanitizeLlmOutput readings llmOutput

            llmInputSha <- liftEffect $ sha256Hex (llmInstructions <> "\n\n" <> input)
            llmOutputSha <- liftEffect $ sha256Hex (stringifyPretty (encodeLlmOutput sanitized))

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

        log "Generating heterodox reading…"

        excursusResult <-
          attempt $
            callOpenAiExcursus
              { model
              , instructions: heterodoxPrompt
              , input
              , temperature: 0.7
              }

        withExcursus <- case excursusResult of
          Left e -> do
            log ("OpenAI heterodox reading failed; continuing without heterodox reading. " <> show e)
            pure base
          Right excursusText -> do
            let heterodoxText = trim excursusText
            llmInputSha <- liftEffect $ sha256Hex (heterodoxPrompt <> "\n\n" <> input)
            llmOutputSha <- liftEffect $ sha256Hex heterodoxText
            pure base
              { commentary = base.commentary { excursus = heterodoxText }
              , calls =
                  base.calls
                    <> [ { name: "heterodox_reading"
                         , model
                         , inputSha256: llmInputSha
                         , outputSha256: llmOutputSha
                         }
                       ]
              }

        log "Generating semina verbi…"

        seminaResult <-
          attempt $
            callOpenAiSeminaVerbi
              { model
              , instructions: seminaVerbiPrompt
              , input
              , temperature: 0.6
              }

        case seminaResult of
          Left e -> do
            log ("OpenAI semina verbi failed; continuing without semina verbi. " <> show e)
            pure withExcursus
          Right seminaText -> do
            let seminaVerbi = trim seminaText
            llmInputSha <- liftEffect $ sha256Hex (seminaVerbiPrompt <> "\n\n" <> input)
            llmOutputSha <- liftEffect $ sha256Hex seminaVerbi
            pure withExcursus
              { commentary = withExcursus.commentary { seminaVerbi = seminaVerbi }
              , calls =
                  withExcursus.calls
                    <> [ { name: "semina_verbi"
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
          , hoursOfPrayer
          , marginalia
          , commentary
          , llm:
              { generatedAt
              , calls
              }
          }

    writeOutputs artifact

defaultHoursOfPrayer :: Array HourEntry
defaultHoursOfPrayer =
  [ { key: "matins", label: "Matins", hourLocal: 0, minuteLocal: 0, prayer: "Lord, open my lips, and my mouth shall declare your praise.", source: "fallback" }
  , { key: "lauds", label: "Lauds", hourLocal: 6, minuteLocal: 0, prayer: "Blessed are you, Lord, in the light of the new day.", source: "fallback" }
  , { key: "terce", label: "Terce", hourLocal: 9, minuteLocal: 0, prayer: "Come, Holy Spirit, and lighten our work in truth.", source: "fallback" }
  , { key: "sext", label: "Sext", hourLocal: 12, minuteLocal: 0, prayer: "God, come to my assistance. Lord, make haste to help me.", source: "fallback" }
  , { key: "none", label: "Nones", hourLocal: 15, minuteLocal: 0, prayer: "Stay with us, Lord, in the heat and trial of this day.", source: "fallback" }
  , { key: "vespers", label: "Vespers", hourLocal: 18, minuteLocal: 0, prayer: "Let my prayer rise before you like incense this evening.", source: "fallback" }
  , { key: "compline", label: "Compline", hourLocal: 21, minuteLocal: 0, prayer: "Into your hands, Lord, I commend my spirit.", source: "fallback" }
  ]

officeOptionsForKey :: OfficePayload -> String -> Array OfficeOption
officeOptionsForKey payload = case _ of
  "matins" -> payload.officium
  "lauds" -> payload.laudes
  "terce" -> payload.tertia
  "sext" -> payload.sexta
  "none" -> payload.nona
  "vespers" -> payload.vesperae
  "compline" -> payload.completorium
  _ -> []

cyclePriority :: String -> Int
cyclePriority cycleCode =
  let
    cycleUpper = String.toUpper cycleCode
  in
    if String.contains (Pattern "MEMORY_PROPER") cycleUpper then 5
    else if String.contains (Pattern "MEMORY") cycleUpper then 4
    else if String.contains (Pattern "SOLEMNITY") cycleUpper then 3
    else if String.contains (Pattern "FEAST") cycleUpper then 2
    else if cycleUpper == "ANY" then 0
    else 1

officeOptionText :: OfficeOption -> String
officeOptionText option =
  let
    finalPrayer = trim option.finalPrayer
  in
    if finalPrayer /= "" then finalPrayer else trim option.reading

preferOfficeOption :: OfficeOption -> OfficeOption -> Boolean
preferOfficeOption candidate incumbent =
  let
    candidatePriority = cyclePriority candidate.cycle
    incumbentPriority = cyclePriority incumbent.cycle
    candidateText = officeOptionText candidate
    incumbentText = officeOptionText incumbent
  in
    if candidatePriority > incumbentPriority then true
    else if candidatePriority < incumbentPriority then false
    else if candidateText /= "" && incumbentText == "" then true
    else if candidateText == "" && incumbentText /= "" then false
    else CodeUnits.length candidateText > CodeUnits.length incumbentText

choosePreferredOption :: Array OfficeOption -> Maybe OfficeOption
choosePreferredOption options =
  options # Array.foldl step Nothing
  where
  step :: Maybe OfficeOption -> OfficeOption -> Maybe OfficeOption
  step Nothing option = Just option
  step (Just incumbent) option =
    if preferOfficeOption option incumbent then
      Just option
    else
      Just incumbent

renderTranslationInput :: String -> OfficeOption -> String -> String
renderTranslationInput officeLabel option text =
  String.joinWith "\n" $
    Array.filter (_ /= "")
      [ "Office: " <> officeLabel
      , if option.id == "" then "" else "Breviarium id: " <> option.id
      , if option.cycle == "" then "" else "Cycle: " <> option.cycle
      , if option.readingRef == "" then "" else "Reference: " <> option.readingRef
      , ""
      , "Spanish text:"
      , text
      ]

translateOfficePrayer
  :: Maybe String
  -> String
  -> String
  -> Maybe OfficeOption
  -> String
  -> Aff { prayer :: String, translated :: Boolean }
translateOfficePrayer openAiKey model officeLabel maybeOption sourcePrayer =
  case openAiKey of
    Nothing -> pure { prayer: sourcePrayer, translated: false }
    Just _ ->
      case maybeOption of
        Nothing -> pure { prayer: sourcePrayer, translated: false }
        Just option -> do
          translationResult <-
            attempt $
              callOpenAiTranslation
                { model
                , instructions: hoursTranslationPrompt
                , input: renderTranslationInput officeLabel option sourcePrayer
                , temperature: 0.0
                }

          case translationResult of
            Left e -> do
              log ("OpenAI translation failed for " <> officeLabel <> "; keeping Breviarium Spanish. " <> show e)
              pure { prayer: sourcePrayer, translated: false }
            Right translatedText -> do
              let clean = trim translatedText
              if clean == "" then
                pure { prayer: sourcePrayer, translated: false }
              else
                pure { prayer: clean, translated: true }

buildHoursOfPrayer :: String -> String -> Maybe String -> Aff (Array HourEntry)
buildHoursOfPrayer dateIso model openAiKey = do
  breviariumResult <- attempt (getOfficePayload dateIso)
  case breviariumResult of
    Left e -> do
      log ("Breviarium lookup failed; using fallback hours. " <> show e)
      pure defaultHoursOfPrayer
    Right payload ->
      defaultHoursOfPrayer # traverse \fallbackRow -> do
        let
          chosen = choosePreferredOption (officeOptionsForKey payload fallbackRow.key)
          chosenPrayer = case chosen of
            Nothing -> fallbackRow.prayer
            Just option ->
              let picked = officeOptionText option
              in if picked == "" then fallbackRow.prayer else picked

        translated <- translateOfficePrayer openAiKey model fallbackRow.label chosen chosenPrayer
        let
          sourceTag = case chosen of
            Nothing -> "fallback"
            Just _ ->
              if translated.translated then "breviarium+openai" else "breviarium"
        pure fallbackRow { prayer = translated.prayer, source = sourceTag }

fetchReadings :: FeedItem -> Aff (Array Reading)
fetchReadings item = do
  item.readings # traverse \r -> do
    api <- fetchBibleReading r.bibleApiReference
    pure
      { kind: readingKindFromString r.kind
      , heading: r.heading
      , reference: api.reference
      , bibleApiReference: r.bibleApiReference
      , translation: api.translation
      , lineRefs: api.lineRefs
      , lines: api.lines
      }

preflightFeed :: Array FeedItem -> Aff Unit
preflightFeed items = do
  log ("Preflight: validating " <> show (Array.length items) <> " feed item(s)…")
  results <- items # traverse \item -> do
    checks <- item.readings # traverse \r -> do
      res <- attempt (fetchBibleReading r.bibleApiReference)
      pure { kind: r.kind, ref: r.bibleApiReference, result: res }
    pure { item, checks }

  let
    failures =
      results # Array.concatMap \entry ->
        entry.checks # Array.mapMaybe \check ->
          case check.result of
            Left e ->
              Just
                { date: entry.item.date
                , title: entry.item.title
                , kind: check.kind
                , ref: check.ref
                , err: show e
                }
            Right _ -> Nothing

    formatFailure f =
      let
        label =
          if f.date == "" then f.title else f.date <> " " <> f.title
      in
        "- " <> label <> " (" <> f.kind <> " " <> f.ref <> "): " <> f.err

  if Array.length failures == 0 then
    log "Preflight OK: all feed readings resolved."
  else
    throwError (error ("Preflight failed:\n" <> String.joinWith "\n" (failures <#> formatFailure)))

readingKindFromString :: String -> ReadingKind
readingKindFromString = case _ of
  "gospel" -> gospelKind
  _ -> firstReadingKind

preferredModel :: Effect (Maybe String)
preferredModel = do
  let
    normalize m =
      m >>= \s -> case trim s of
        "" -> Nothing
        t -> Just t

  explicit <- normalize <$> Process.lookupEnv "VERBUM_OPENAI_MODEL"
  case explicit of
    Just m -> pure (Just m)
    Nothing -> normalize <$> Process.lookupEnv "OPENAI_MODEL"

shouldTranslateHours :: Effect Boolean
shouldTranslateHours = do
  raw <- Process.lookupEnv "VERBUM_TRANSLATE_HOURS"
  pure $ case raw of
    Nothing -> true
    Just value ->
      let normalized = String.toLower (trim value)
      in
        normalized /= "0"
          && normalized /= "false"
          && normalized /= "no"
          && normalized /= "off"

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

emptyCommentary :: Commentary
emptyCommentary =
  { reading: []
  , gospel: []
  , synthesis: ""
  , excursus: ""
  , seminaVerbi: ""
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
      , excursus: trim llmOutput.commentary.excursus
      , seminaVerbi: trim llmOutput.commentary.seminaVerbi
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
  entries <- readDir "public/data"
  pure $
    entries
      # Array.mapMaybe extractDateFromDataFilename
      # Array.sort
      # Array.reverse

writeOutputs :: Artifact -> Aff Unit
writeOutputs artifact = do
  let json = stringifyPretty (encodeArtifact artifact)

  liftEffect do
    ensureDir "public"
    ensureDir "public/data"
    ensureDir "public/archive"

    writeTextFile ("public/data/" <> artifact.date <> ".json") json

  dates <- liftEffect listDataDates
  let archiveJson =
        stringifyPretty $
          "dates" := dates
            ~> jsonEmptyObject

  rootHtml <- liftEffect $
    renderArtifactPage
      { assetPrefix: ""
      , homeHref: "./"
      , archiveHref: "archive/"
      , permalinkHref: "d/" <> artifact.date <> "/"
      }
      artifact

  archiveHtml <- liftEffect $
    renderArchivePage
      { assetPrefix: "../"
      , homeHref: "../"
      , dayHrefPrefix: "../"
      }
      dates

  dayPermalinkHtmlStatic <- liftEffect $
    renderArtifactPage
      { assetPrefix: "../../"
      , homeHref: "../../"
      , archiveHref: "../../archive/"
      , permalinkHref: "./"
      }
      artifact

  dayLegacyHtmlStatic <- liftEffect $
    renderArtifactPage
      { assetPrefix: "../"
      , homeHref: "../"
      , archiveHref: "../archive/"
      , permalinkHref: "../d/" <> artifact.date <> "/"
      }
      artifact

  dayPermalinkHtmlShell <- liftEffect $
    renderAppShellPage
      { assetPrefix: "../../"
      , pageTitle: "Verbum Diei"
      , defaultView: "latest"
      }

  dayLegacyHtmlShell <- liftEffect $
    renderAppShellPage
      { assetPrefix: "../"
      , pageTitle: "Verbum Diei"
      , defaultView: "latest"
      }

  liftEffect do
    writeTextFile "public/data/archive.json" archiveJson
    writeTextFile "public/index.html" rootHtml
    writeTextFile "public/archive/index.html" archiveHtml
    _ <- dates # traverse \date -> do
      ensureDir ("public/d/" <> date)
      ensureDir ("public/" <> date)
      if date == artifact.date then do
        writeTextFile ("public/d/" <> date <> "/index.html") dayPermalinkHtmlStatic
        writeTextFile ("public/" <> date <> "/index.html") dayLegacyHtmlStatic
      else do
        writeTextFile ("public/d/" <> date <> "/index.html") dayPermalinkHtmlShell
        writeTextFile ("public/" <> date <> "/index.html") dayLegacyHtmlShell
    pure unit

  log ("Wrote static latest pages + archive, date routes, public/data/archive.json, and public/data/" <> artifact.date <> ".json")

argValue :: String -> Array String -> Maybe String
argValue key argv =
  case Array.findIndex (_ == key) argv of
    Just i -> Array.index argv (i + 1)
    Nothing -> findEquals key argv

hasFlag :: String -> Array String -> Boolean
hasFlag key argv =
  case Array.findIndex (_ == key) argv of
    Just _ -> true
    Nothing -> false

findEquals :: String -> Array String -> Maybe String
findEquals key argv =
  argv
    # Array.findMap \arg -> case split (Pattern "=") arg of
        [ k, v ] | k == key -> Just v
        _ -> Nothing
