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
  , encodeArtifact
  , encodeCommentary
  , encodeMarginalNote
  , firstReadingKind
  , gospelKind
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
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
    ~> "marginalia" := map encodeMarginalNote artifact.marginalia
    ~> "commentary" := encodeCommentary artifact.commentary
    ~> "llm" := encodeLlmMeta artifact.llm
    ~> jsonEmptyObject
