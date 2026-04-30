module VerbumDiei.Bible
  ( BibleReading
  , fetchBibleReading
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json, toArray, toNull, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException, try)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as FO
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import VerbumDiei.Artifact (Translation)
import VerbumDiei.Bible.Citation (CitationRef(..), VerseRef, expandCitation, parseCitation, parseReference)

type BibleReading =
  { reference :: String
  , translation :: Translation
  , lineRefs :: Array String
  , lines :: Array String
  }

type BibleData =
  { translation :: Translation
  , books :: FO.Object (Array (Array String))
  , bookByKey :: Array { key :: String, name :: String }
  }

type ChapterFold =
  { fromChapter :: Int
  , toChapter :: Int
  }

type ChapterOffset =
  { chapter :: Int
  , offset :: Int
  }

-- | Maps an entire chapter to a different chapter with optional verse offset
-- | Used for cases like Joel where Vulgate 3:x = Protestant 2:(x+27)
type ChapterRemap =
  { fromChapter :: Int
  , toChapter :: Int
  , verseOffset :: Int  -- added to verse number (can be negative)
  }

-- | Maps a specific verse to another verse in the same chapter
-- | Used when DRA collapses two Nova Vulgata verses into one
type VerseAlias =
  { chapter :: Int
  , fromVerse :: Int
  , toVerse :: Int
  }

type VerseMap =
  { book :: String
  , folds :: Array ChapterFold
  , offsets :: Array ChapterOffset
  , remaps :: Array ChapterRemap
  , aliases :: Array VerseAlias
  }

dataRef :: Ref.Ref (Maybe BibleData)
dataRef = unsafePerformEffect (Ref.new Nothing)

fetchBibleReading :: String -> Aff BibleReading
fetchBibleReading reference = do
  case parseReference reference of
    Left errMsg ->
      throwError (error errMsg)
    Right { book, citation } ->
      case parseCitation citation of
        Left errMsg ->
          throwError (error errMsg)
        Right segments -> do
          let citationRefs = expandCitation segments
          if Array.null citationRefs then
            throwError (error ("Could not parse citation: " <> citation))
          else do
            result <- liftEffect $ fetchBibleReadingFromData book citation citationRefs
            case result of
              Left errMsg -> throwError (error errMsg)
              Right reading -> pure reading

fetchBibleReadingFromData :: String -> String -> Array CitationRef -> Effect (Either String BibleReading)
fetchBibleReadingFromData bookRaw citation citationRefs = do
  data' <- loadData
  case resolveBookNameIn data'.bookByKey bookRaw of
    Nothing ->
      pure (Left ("Unknown book: " <> bookRaw))
    Just book -> do
      case FO.lookup book data'.books of
        Nothing -> pure (Left ("Unknown book: " <> book))
        Just chapters -> do
          let expandedRefs = expandCitationRefs chapters citationRefs
          let
            sameChapter = case Array.head expandedRefs of
              Nothing -> true
              Just firstRef -> Array.all (\r -> r.chapter == firstRef.chapter) expandedRefs
            lineRefs = expandedRefs <#> \r ->
              if sameChapter then
                show r.verse
              else
                show r.chapter <> ":" <> show r.verse
          case traverse (lookupVerse chapters book) expandedRefs of
            Left errMsg -> pure (Left errMsg)
            Right lines ->
              pure
                (Right
                  { reference: book <> " " <> citation
                  , translation: data'.translation
                  , lineRefs
                  , lines
                  })

expandCitationRefs :: Array (Array String) -> Array CitationRef -> Array VerseRef
expandCitationRefs chapters refs =
  refs >>= \ref ->
    case ref of
      Verse verseRef -> [ verseRef ]
      ExplicitCrossChapterRange start end ->
        [ start ] <> generateCrossChapterGap start end <> [ end ]
  where
  generateCrossChapterGap start end =
    let
      startChapterArr = Array.index chapters (start.chapter - 1)
      startMaxVerse = fromMaybe 0 ((\arr -> Array.length arr) <$> startChapterArr)
      remainingInStart =
        if start.verse + 1 <= startMaxVerse then
          Array.range (start.verse + 1) startMaxVerse
            <#> \v -> { chapter: start.chapter, verse: v }
        else
          []

      intermediateChapters =
        if start.chapter + 1 <= end.chapter - 1 then
          Array.range (start.chapter + 1) (end.chapter - 1)
        else
          []
      intermediateVerses = intermediateChapters >>= \ch ->
        let chapterArr = Array.index chapters (ch - 1)
            maxVerse = fromMaybe 0 ((\arr -> Array.length arr) <$> chapterArr)
        in Array.range 1 maxVerse <#> \v -> { chapter: ch, verse: v }

      prefixInEnd =
        if 1 <= end.verse - 1 then
          Array.range 1 (end.verse - 1)
            <#> \v -> { chapter: end.chapter, verse: v }
        else
          []
    in
      remainingInStart <> intermediateVerses <> prefixInEnd

lookupVerse :: Array (Array String) -> String -> VerseRef -> Either String String
lookupVerse chapters book ref =
  -- Try mapping first (handles versification differences where verse exists at different location)
  case mapVerseReference chapters book ref.chapter ref.verse of
    Just mapped ->
      case getVerseText chapters mapped.chapter mapped.verse of
        Just verse -> Right verse
        Nothing ->
          Left ("Missing verse text for " <> book <> " " <> show ref.chapter <> ":" <> show ref.verse <> " (mapped to " <> show mapped.chapter <> ":" <> show mapped.verse <> ")")
    Nothing ->
      -- No mapping applies, try direct lookup
      case getVerseText chapters ref.chapter ref.verse of
        Just verse -> Right verse
        Nothing ->
          Left ("Missing verse text for " <> book <> " " <> show ref.chapter <> ":" <> show ref.verse)

getVerseText :: Array (Array String) -> Int -> Int -> Maybe String
getVerseText chapters chapter verse = do
  chapterArr <- Array.index chapters (chapter - 1)
  Array.index chapterArr (verse - 1)

mapVerseReference :: Array (Array String) -> String -> Int -> Int -> Maybe VerseRef
mapVerseReference chapters book chapter verse = do
  maps <- Array.find (\m -> m.book == book) verseMaps
  -- Check remaps first (entire chapter relocations like Joel)
  case foldMapMaybe (applyRemap chapters chapter verse) maps.remaps of
    Just mapped -> Just mapped
    Nothing ->
      -- Then check folds (overflow verses like Isaiah 8:23 → 9:1)
      case foldMapMaybe (applyFold chapters chapter verse) maps.folds of
        Just mapped -> Just mapped
        Nothing ->
          case foldMapMaybe (applyOffset chapters chapter verse) maps.offsets of
            Just mapped -> Just mapped
            Nothing -> foldMapMaybe (applyAlias chapter verse) maps.aliases

applyFold :: Array (Array String) -> Int -> Int -> ChapterFold -> Maybe VerseRef
applyFold chapters chapter verse fold =
  if chapter /= fold.fromChapter then
    Nothing
  else do
    fromArr <- Array.index chapters (fold.fromChapter - 1)
    toArr <- Array.index chapters (fold.toChapter - 1)
    let offset = Array.length fromArr
    if verse <= offset then
      Nothing
    else
      let mappedVerse = verse - offset
      in if mappedVerse < 1 || mappedVerse > Array.length toArr then
           Nothing
         else
           Just { chapter: fold.toChapter, verse: mappedVerse }

applyOffset :: Array (Array String) -> Int -> Int -> ChapterOffset -> Maybe VerseRef
applyOffset chapters chapter verse mapping =
  if chapter /= mapping.chapter then
    Nothing
  else do
    chapterArr <- Array.index chapters (chapter - 1)
    let mappedVerse = verse - mapping.offset
    if mappedVerse < 1 || mappedVerse > Array.length chapterArr then
      Nothing
    else
      Just { chapter, verse: mappedVerse }

applyRemap :: Array (Array String) -> Int -> Int -> ChapterRemap -> Maybe VerseRef
applyRemap chapters chapter verse remap =
  if chapter /= remap.fromChapter then
    Nothing
  else do
    targetArr <- Array.index chapters (remap.toChapter - 1)
    let mappedVerse = verse + remap.verseOffset
    if mappedVerse < 1 || mappedVerse > Array.length targetArr then
      Nothing
    else
      Just { chapter: remap.toChapter, verse: mappedVerse }

applyAlias :: Int -> Int -> VerseAlias -> Maybe VerseRef
applyAlias chapter verse alias =
  if chapter == alias.chapter && verse == alias.fromVerse then
    Just { chapter, verse: alias.toVerse }
  else
    Nothing

foldMapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
foldMapMaybe f arr =
  Array.findMap f arr

verseMaps :: Array VerseMap
verseMaps =
  [ { book: "Malachias"
    , folds: [ { fromChapter: 3, toChapter: 4 } ]
    , offsets: []
    , remaps: []
    , aliases: []
    }
  , { book: "Zechariah"
    , folds: []
    , offsets: [ { chapter: 2, offset: 4 } ]
    , remaps: []
    , aliases: []
    }
  , { book: "Isaiah"
    , folds: [ { fromChapter: 8, toChapter: 9 } ]
    , offsets: []
    , remaps: []
    , aliases: []
    }
  , { book: "Joel"
    , folds: []
    , offsets: []
    , remaps:
        [ { fromChapter: 3, toChapter: 2, verseOffset: 27 }  -- Vulgate 3:x → DRA 2:(x+27)
        , { fromChapter: 4, toChapter: 3, verseOffset: 0 }   -- Vulgate 4:x → DRA 3:x
        ]
    , aliases: []
    }
  , { book: "Mark"
    , folds: []
    , offsets: []
    , remaps: []
    , aliases: [ { chapter: 4, fromVerse: 41, toVerse: 40 } ]  -- DRA combines NV 40+41 into verse 40
    }
  ]

loadData :: Effect BibleData
loadData = do
  cached <- Ref.read dataRef
  case cached of
    Just data' -> pure data'
    Nothing -> do
      path <- dataPath
      rawResult <- try (FS.readTextFile UTF8 path)
      raw <- case rawResult of
        Left err -> throwException err
        Right text -> pure text
      case parseBibleData raw of
        Left errMsg -> throwException (error errMsg)
        Right data' -> do
          Ref.write (Just data') dataRef
          pure data'

dataPath :: Effect String
dataPath = do
  cwd <- Process.cwd
  pure $ Path.concat [ cwd, "assets", "bible", "dra1899.json" ]

parseBibleData :: String -> Either String BibleData
parseBibleData raw = do
  json <- jsonParser raw
  root <- note "Expected top-level object" (toObject json)
  booksJson <- note "Missing books" (FO.lookup "books" root)
  booksObj <- note "Invalid books" (toObject booksJson)
  books <- decodeBooks booksObj
  let translation = decodeTranslation (FO.lookup "translation" root)
  let bookByKey = buildBookByKey (FO.keys books)
  let withAliases = applyAliases bookByKey
  Right { translation, books, bookByKey: withAliases }

decodeBooks :: FO.Object Json -> Either String (FO.Object (Array (Array String)))
decodeBooks booksObj = do
  let pairs = FO.toUnfoldable booksObj :: Array (Tuple String Json)
  decoded <- traverse decodeBook pairs
  pure (FO.fromFoldable decoded)

decodeBook :: Tuple String Json -> Either String (Tuple String (Array (Array String)))
decodeBook (Tuple name value) =
  case decodeChapters value of
    Nothing -> Left ("Invalid book data for " <> name)
    Just chapters -> Right (Tuple name chapters)

decodeChapters :: Json -> Maybe (Array (Array String))
decodeChapters value = do
  chapters <- toArray value
  traverse decodeChapter chapters

decodeChapter :: Json -> Maybe (Array String)
decodeChapter value = do
  verses <- toArray value
  traverse decodeVerse verses

decodeVerse :: Json -> Maybe String
decodeVerse value =
  case toString value of
    Just verse -> Just verse
    Nothing ->
      case toNull value of
        Just _ -> Just ""
        Nothing -> Nothing

decodeTranslation :: Maybe Json -> Translation
decodeTranslation maybeJson =
  case maybeJson >>= toObject of
    Nothing -> defaultTranslation
    Just obj ->
      { id: fromMaybe defaultTranslation.id (FO.lookup "id" obj >>= toString)
      , name: fromMaybe defaultTranslation.name (FO.lookup "name" obj >>= toString)
      , note: fromMaybe defaultTranslation.note (FO.lookup "note" obj >>= toString)
      }

defaultTranslation :: Translation
defaultTranslation =
  { id: "dra"
  , name: "Douay-Rheims 1899 American Edition"
  , note: "Public Domain"
  }

applyAliases :: Array { key :: String, name :: String } -> Array { key :: String, name :: String }
applyAliases base =
  Array.foldl addAlias base aliasPairs
  where
  addAlias acc (Tuple fromName toName) =
    case resolveBookNameIn acc toName of
      Just canonical ->
        let key = normalizeBookKey fromName
        in acc <> [ { key, name: canonical } ]
      Nothing -> acc

buildBookByKey :: Array String -> Array { key :: String, name :: String }
buildBookByKey names =
  names <#> \name -> { key: normalizeBookKey name, name }

resolveBookNameIn :: Array { key :: String, name :: String } -> String -> Maybe String
resolveBookNameIn byKey input =
  let key = normalizeBookKey input
  in Array.findMap (\entry -> if entry.key == key then Just entry.name else Nothing) byKey

aliasPairs :: Array (Tuple String String)
aliasPairs =
  -- Alternate canonical names
  [ Tuple "ecclesiasticus" "Sirach"
  , Tuple "sirach" "Sirach"
  , Tuple "canticleofcanticles" "Song of Songs"
  , Tuple "songofsongs" "Song of Songs"
  , Tuple "apocalypse" "Revelation"
  , Tuple "revelation" "Revelation"
  , Tuple "tobias" "Tobit"
  , Tuple "tobit" "Tobit"
  , Tuple "malachi" "Malachias"
  , Tuple "malachias" "Malachias"
  , Tuple "josue" "Joshua"
  , Tuple "joshua" "Joshua"
  , Tuple "paralipomenon" "Chronicles"
  , Tuple "machabees" "Maccabees"
  -- Bare ordinal-book names default to "1 X"
  , Tuple "samuel" "1 Samuel"
  , Tuple "kings" "1 Kings"
  , Tuple "chronicles" "1 Chronicles"
  , Tuple "maccabees" "1 Maccabees"
  , Tuple "corinthians" "1 Corinthians"
  , Tuple "thessalonians" "1 Thessalonians"
  , Tuple "timothy" "1 Timothy"
  , Tuple "peter" "1 Peter"
  -- OT abbreviations
  , Tuple "gen" "Genesis"
  , Tuple "ex" "Exodus"
  , Tuple "exod" "Exodus"
  , Tuple "lev" "Leviticus"
  , Tuple "lv" "Leviticus"
  , Tuple "num" "Numbers"
  , Tuple "nm" "Numbers"
  , Tuple "deut" "Deuteronomy"
  , Tuple "dt" "Deuteronomy"
  , Tuple "jos" "Joshua"
  , Tuple "josh" "Joshua"
  , Tuple "judg" "Judges"
  , Tuple "jgs" "Judges"
  , Tuple "jdg" "Judges"
  , Tuple "sam" "1 Samuel"
  , Tuple "kgs" "1 Kings"
  , Tuple "chr" "1 Chronicles"
  , Tuple "chron" "1 Chronicles"
  , Tuple "ezr" "Ezra"
  , Tuple "neh" "Nehemiah"
  , Tuple "tob" "Tobit"
  , Tuple "jdt" "Judith"
  , Tuple "est" "Esther"
  , Tuple "esth" "Esther"
  , Tuple "ps" "Psalms"
  , Tuple "pss" "Psalms"
  , Tuple "prov" "Proverbs"
  , Tuple "prv" "Proverbs"
  , Tuple "eccl" "Ecclesiastes"
  , Tuple "qoh" "Ecclesiastes"
  , Tuple "song" "Song of Songs"
  , Tuple "sg" "Song of Songs"
  , Tuple "cant" "Song of Songs"
  , Tuple "wis" "Wisdom"
  , Tuple "sir" "Sirach"
  , Tuple "is" "Isaiah"
  , Tuple "isa" "Isaiah"
  , Tuple "jer" "Jeremiah"
  , Tuple "lam" "Lamentations"
  , Tuple "bar" "Baruch"
  , Tuple "ezek" "Ezekiel"
  , Tuple "ez" "Ezekiel"
  , Tuple "dan" "Daniel"
  , Tuple "dn" "Daniel"
  , Tuple "hos" "Hosea"
  , Tuple "jl" "Joel"
  , Tuple "am" "Amos"
  , Tuple "ob" "Obadiah"
  , Tuple "obad" "Obadiah"
  , Tuple "jon" "Jonah"
  , Tuple "mic" "Micah"
  , Tuple "mi" "Micah"
  , Tuple "nah" "Nahum"
  , Tuple "na" "Nahum"
  , Tuple "hab" "Habakkuk"
  , Tuple "zeph" "Zephaniah"
  , Tuple "zep" "Zephaniah"
  , Tuple "hag" "Haggai"
  , Tuple "hg" "Haggai"
  , Tuple "zech" "Zechariah"
  , Tuple "zec" "Zechariah"
  , Tuple "mal" "Malachias"
  , Tuple "macc" "1 Maccabees"
  , Tuple "mac" "1 Maccabees"
  -- NT abbreviations
  , Tuple "mt" "Matthew"
  , Tuple "matt" "Matthew"
  , Tuple "mk" "Mark"
  , Tuple "mrk" "Mark"
  , Tuple "lk" "Luke"
  , Tuple "luk" "Luke"
  , Tuple "jn" "John"
  , Tuple "jhn" "John"
  , Tuple "joh" "John"
  , Tuple "rom" "Romans"
  , Tuple "cor" "1 Corinthians"
  , Tuple "gal" "Galatians"
  , Tuple "eph" "Ephesians"
  , Tuple "phil" "Philippians"
  , Tuple "col" "Colossians"
  , Tuple "thess" "1 Thessalonians"
  , Tuple "thes" "1 Thessalonians"
  , Tuple "tim" "1 Timothy"
  , Tuple "tit" "Titus"
  , Tuple "phlm" "Philemon"
  , Tuple "philem" "Philemon"
  , Tuple "heb" "Hebrews"
  , Tuple "jas" "James"
  , Tuple "pet" "1 Peter"
  , Tuple "pt" "1 Peter"
  , Tuple "rev" "Revelation"
  , Tuple "rv" "Revelation"
  ]

normalizeBookKey :: String -> String
normalizeBookKey input =
  let
    tokens = normalizeTokens (tokenizeWords input)
    trimmed = Array.filter (\t -> t /= "saint" && t /= "st") tokens
  in
    joinWith "" trimmed

normalizeTokens :: Array String -> Array String
normalizeTokens tokens =
  case Array.uncons tokens of
    Just { head, tail } ->
      case romanToDigit head of
        Just n -> [ n ] <> tail
        Nothing -> tokens
    Nothing -> tokens

romanToDigit :: String -> Maybe String
romanToDigit token =
  case token of
    "i" -> Just "1"
    "ii" -> Just "2"
    "iii" -> Just "3"
    "iv" -> Just "4"
    _ -> Nothing

tokenizeWords :: String -> Array String
tokenizeWords s =
  let
    chars = CodeUnits.toCharArray s
    push acc current =
      if Array.null current then
        acc
      else
        CodeUnits.fromCharArray (Array.reverse current) `cons` acc
    go remaining current acc =
      case Array.uncons remaining of
        Nothing -> Array.reverse (push acc current)
        Just { head: c, tail: rest } ->
          if isAlphaNumChar c then
            go rest (toLowerChar c `cons` current) acc
          else
            go rest [] (push acc current)
  in
    go chars [] []

isAlphaNumChar :: Char -> Boolean
isAlphaNumChar c =
  isAlphaChar c || isDigitChar c

isDigitChar :: Char -> Boolean
isDigitChar c =
  let code = Char.toCharCode c
  in code >= 48 && code <= 57

isAlphaChar :: Char -> Boolean
isAlphaChar c =
  let code = Char.toCharCode c
  in (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

toLowerChar :: Char -> Char
toLowerChar c =
  let code = Char.toCharCode c
  in if code >= 65 && code <= 90 then
       case Char.fromCharCode (code + 32) of
         Just lowered -> lowered
         Nothing -> c
     else
       c

note :: forall a. String -> Maybe a -> Either String a
note errMsg maybeValue =
  case maybeValue of
    Just value -> Right value
    Nothing -> Left errMsg

cons :: forall a. a -> Array a -> Array a
cons x xs = [ x ] <> xs
