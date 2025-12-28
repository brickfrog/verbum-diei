module VerbumDiei.Bible
  ( BibleReading
  , fetchBibleReading
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json, toArray, toObject, toString)
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
import VerbumDiei.Bible.Citation (VerseRef, expandCitation, parseCitation, parseReference)

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

type VerseMap =
  { book :: String
  , folds :: Array ChapterFold
  , offsets :: Array ChapterOffset
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
          let refs = expandCitation segments
          if Array.null refs then
            throwError (error ("Could not parse citation: " <> citation))
          else do
            result <- liftEffect $ fetchBibleReadingFromData book citation refs
            case result of
              Left errMsg -> throwError (error errMsg)
              Right reading -> pure reading

fetchBibleReadingFromData :: String -> String -> Array VerseRef -> Effect (Either String BibleReading)
fetchBibleReadingFromData bookRaw citation refs = do
  data' <- loadData
  case resolveBookNameIn data'.bookByKey bookRaw of
    Nothing ->
      pure (Left ("Unknown book: " <> bookRaw))
    Just book -> do
      case FO.lookup book data'.books of
        Nothing -> pure (Left ("Unknown book: " <> book))
        Just chapters -> do
          let
            sameChapter = case Array.head refs of
              Nothing -> true
              Just firstRef -> Array.all (\r -> r.chapter == firstRef.chapter) refs
            lineRefs = refs <#> \r ->
              if sameChapter then
                show r.verse
              else
                show r.chapter <> ":" <> show r.verse
          case traverse (lookupVerse chapters book) refs of
            Left errMsg -> pure (Left errMsg)
            Right lines ->
              pure
                (Right
                  { reference: book <> " " <> citation
                  , translation: data'.translation
                  , lineRefs
                  , lines
                  })

lookupVerse :: Array (Array String) -> String -> VerseRef -> Either String String
lookupVerse chapters book ref =
  case getVerseText chapters ref.chapter ref.verse of
    Just verse -> Right verse
    Nothing ->
      case mapVerseReference chapters book ref.chapter ref.verse of
        Just mapped ->
          case getVerseText chapters mapped.chapter mapped.verse of
            Just verse -> Right verse
            Nothing ->
              Left ("Missing verse text for " <> book <> " " <> show ref.chapter <> ":" <> show ref.verse)
        Nothing ->
          Left ("Missing verse text for " <> book <> " " <> show ref.chapter <> ":" <> show ref.verse)

getVerseText :: Array (Array String) -> Int -> Int -> Maybe String
getVerseText chapters chapter verse = do
  chapterArr <- Array.index chapters (chapter - 1)
  Array.index chapterArr (verse - 1)

mapVerseReference :: Array (Array String) -> String -> Int -> Int -> Maybe VerseRef
mapVerseReference chapters book chapter verse = do
  maps <- Array.find (\m -> m.book == book) verseMaps
  case foldMapMaybe (applyFold chapters chapter verse) maps.folds of
    Just mapped -> Just mapped
    Nothing -> foldMapMaybe (applyOffset chapters chapter verse) maps.offsets

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

foldMapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
foldMapMaybe f arr =
  Array.findMap f arr

verseMaps :: Array VerseMap
verseMaps =
  [ { book: "Malachias"
    , folds: [ { fromChapter: 3, toChapter: 4 } ]
    , offsets: []
    }
  , { book: "Zechariah"
    , folds: []
    , offsets: [ { chapter: 2, offset: 4 } ]
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
  traverse toString verses

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
  , Tuple "samuel" "1 Samuel"
  , Tuple "kings" "1 Kings"
  , Tuple "chronicles" "1 Chronicles"
  , Tuple "maccabees" "1 Maccabees"
  , Tuple "corinthians" "1 Corinthians"
  , Tuple "thessalonians" "1 Thessalonians"
  , Tuple "timothy" "1 Timothy"
  , Tuple "peter" "1 Peter"
  , Tuple "mt" "Matthew"
  , Tuple "matt" "Matthew"
  , Tuple "mk" "Mark"
  , Tuple "mrk" "Mark"
  , Tuple "lk" "Luke"
  , Tuple "luk" "Luke"
  , Tuple "jn" "John"
  , Tuple "jhn" "John"
  , Tuple "joh" "John"
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
