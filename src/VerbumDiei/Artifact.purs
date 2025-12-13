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
  , firstReadingKind
  , gospelKind
  ) where

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
