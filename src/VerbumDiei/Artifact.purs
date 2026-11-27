module VerbumDiei.Artifact
  ( Artifact
  , Celebration
  , Commentary
  , CommentNote
  , LlmCallMeta
  , LlmMeta
  , MarginalNote
  , Observances
  , ObservancesMeta
  , Reading
  , ReadingKind
  , Source
  , Translation
  , decodeStoredArtifact
  , defaultHoursOfPrayer
  , encodeArtifact
  , encodeCommentary
  , encodeHourEntry
  , encodeMarginalNote
  , firstReadingKind
  , gospelKind
  , HourEntry
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (JsonDecodeError, fromJsonString)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Data.Argonaut.Encode ((:=), (~>))

type ReadingKind = String

firstReadingKind :: ReadingKind
firstReadingKind = "first"

gospelKind :: ReadingKind
gospelKind = "gospel"

type Source =
  { rssUrl :: String
  , itemUrl :: String
  , title :: String
  , guid :: String
  }

type Translation =
  { id :: String
  , name :: String
  , note :: String
  }

type Reading =
  { kind :: ReadingKind
  , heading :: String
  , reference :: String
  , bibleApiReference :: String
  , translation :: Translation
  , lineRefs :: Array String
  , lines :: Array String
  }

type MarginalNote =
  { readingKind :: ReadingKind
  , lines :: Array Int
  , text :: String
  }

type CommentNote =
  { lines :: Array Int
  , text :: String
  }

type Commentary =
  { reading :: Array CommentNote
  , gospel :: Array CommentNote
  , synthesis :: String
  , excursus :: String
  , seminaVerbi :: String
  }

type HourEntry =
  { key :: String
  , label :: String
  , hourLocal :: Int
  , minuteLocal :: Int
  , prayer :: String
  , source :: String
  }

type ObservancesMeta =
  { season :: String
  , cycle :: String
  , psalterWeek :: String
  }

type Celebration =
  { key :: String
  , name :: String
  , rank :: String
  , source :: String
  , color :: String
  , titles :: Array String
  }

type Observances =
  { meta :: ObservancesMeta
  , celebrations :: Array Celebration
  }

type LlmCallMeta =
  { name :: String
  , model :: String
  , inputSha256 :: String
  , outputSha256 :: String
  }

type LlmMeta =
  { generatedAt :: String
  , calls :: Array LlmCallMeta
  }

type Artifact =
  { date :: String
  , source :: Source
  , observances :: Observances
  , readings :: Array Reading
  , hoursOfPrayer :: Array HourEntry
  , marginalia :: Array MarginalNote
  , commentary :: Commentary
  , llm :: LlmMeta
  }

encodeSource :: Source -> Json
encodeSource source =
  "rssUrl" := source.rssUrl
    ~> "itemUrl" := source.itemUrl
    ~> "title" := source.title
    ~> "guid" := source.guid
    ~> jsonEmptyObject

encodeTranslation :: Translation -> Json
encodeTranslation translation =
  "id" := translation.id
    ~> "name" := translation.name
    ~> "note" := translation.note
    ~> jsonEmptyObject

encodeReading :: Reading -> Json
encodeReading reading =
  "kind" := reading.kind
    ~> "heading" := reading.heading
    ~> "reference" := reading.reference
    ~> "bibleApiReference" := reading.bibleApiReference
    ~> "translation" := encodeTranslation reading.translation
    ~> "lineRefs" := reading.lineRefs
    ~> "lines" := reading.lines
    ~> jsonEmptyObject

encodeMarginalNote :: MarginalNote -> Json
encodeMarginalNote note =
  "readingKind" := note.readingKind
    ~> "lines" := note.lines
    ~> "text" := note.text
    ~> jsonEmptyObject

encodeCommentNote :: CommentNote -> Json
encodeCommentNote note =
  "lines" := note.lines
    ~> "text" := note.text
    ~> jsonEmptyObject

encodeCommentary :: Commentary -> Json
encodeCommentary commentary =
  "reading" := map encodeCommentNote commentary.reading
    ~> "gospel" := map encodeCommentNote commentary.gospel
    ~> "synthesis" := commentary.synthesis
    ~> "excursus" := commentary.excursus
    ~> "seminaVerbi" := commentary.seminaVerbi
    ~> jsonEmptyObject

encodeHourEntry :: HourEntry -> Json
encodeHourEntry hour =
  "key" := hour.key
    ~> "label" := hour.label
    ~> "hourLocal" := hour.hourLocal
    ~> "minuteLocal" := hour.minuteLocal
    ~> "prayer" := hour.prayer
    ~> "source" := hour.source
    ~> jsonEmptyObject

encodeObservancesMeta :: ObservancesMeta -> Json
encodeObservancesMeta meta =
  "season" := meta.season
    ~> "cycle" := meta.cycle
    ~> "psalterWeek" := meta.psalterWeek
    ~> jsonEmptyObject

encodeCelebration :: Celebration -> Json
encodeCelebration celebration =
  "key" := celebration.key
    ~> "name" := celebration.name
    ~> "rank" := celebration.rank
    ~> "source" := celebration.source
    ~> "color" := celebration.color
    ~> "titles" := celebration.titles
    ~> jsonEmptyObject

encodeObservances :: Observances -> Json
encodeObservances observances =
  "meta" := encodeObservancesMeta observances.meta
    ~> "celebrations" := map encodeCelebration observances.celebrations
    ~> jsonEmptyObject

encodeLlmCallMeta :: LlmCallMeta -> Json
encodeLlmCallMeta call =
  "name" := call.name
    ~> "model" := call.model
    ~> "inputSha256" := call.inputSha256
    ~> "outputSha256" := call.outputSha256
    ~> jsonEmptyObject

encodeLlmMeta :: LlmMeta -> Json
encodeLlmMeta meta =
  "generatedAt" := meta.generatedAt
    ~> "calls" := map encodeLlmCallMeta meta.calls
    ~> jsonEmptyObject

encodeArtifact :: Artifact -> Json
encodeArtifact artifact =
  "date" := artifact.date
    ~> "source" := encodeSource artifact.source
    ~> "observances" := encodeObservances artifact.observances
    ~> "readings" := map encodeReading artifact.readings
    ~> "hoursOfPrayer" := map encodeHourEntry artifact.hoursOfPrayer
    ~> "marginalia" := map encodeMarginalNote artifact.marginalia
    ~> "commentary" := encodeCommentary artifact.commentary
    ~> "llm" := encodeLlmMeta artifact.llm
    ~> jsonEmptyObject

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

-- Early editions predate verse labels, prayer hours, and generation timestamps.
decodeStoredArtifact :: String -> Either JsonDecodeError Artifact
decodeStoredArtifact raw = do
  stored <- fromJsonString raw
  pure stored
    { hoursOfPrayer = fromMaybe [] stored.hoursOfPrayer
    , readings = stored.readings <#> \r -> r { lineRefs = fromMaybe [] r.lineRefs }
    , llm = stored.llm { generatedAt = fromMaybe "" stored.llm.generatedAt }
    }
