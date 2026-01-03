module VerbumDiei.Bible.Citation
  ( ChapterSegment
  , VersePart(..)
  , VerseRef
  , expandCitation
  , parseCitation
  , parseReference
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Common (trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)

type VerseRef =
  { chapter :: Int
  , verse :: Int
  }

data VersePart
  = Single Int
  | Range Int Int
  | CrossChapterRange
      { startVerse :: Int
      , endChapter :: Int
      , endVerse :: Int
      }

type ChapterSegment =
  { chapter :: Int
  , parts :: Array VersePart
  }

type Cursor =
  { chars :: Array Char
  , pos :: Int
  }

type Parsed a =
  { value :: a
  , cursor :: Cursor
  }

parseReference :: String -> Either String { book :: String, citation :: String }
parseReference reference = do
  let trimmed = trim reference
  let chars = toCharArray trimmed
  let colonIndex = findIndex (eq ':') chars
  case colonIndex of
    Nothing -> Left ("Could not parse reference: " <> trimmed)
    Just idx -> do
      let beforeColon = Array.slice 0 idx chars
      let spaceIndex = findLastIndex isSpaceChar beforeColon
      case spaceIndex of
        Nothing -> Left ("Could not parse reference: " <> trimmed)
        Just spaceIdx -> do
          let bookRaw = trim (fromCharArray (Array.slice 0 spaceIdx chars))
          let citation = trim (fromCharArray (Array.slice (spaceIdx + 1) (Array.length chars) chars))
          if bookRaw == "" || citation == "" then
            Left ("Could not parse reference: " <> trimmed)
          else
            Right { book: bookRaw, citation }

parseCitation :: String -> Either String (Array ChapterSegment)
parseCitation citation = do
  let cursor = mkCursor citation
  parsed <- parseSegments cursor
  let rest = skipSpaces parsed.cursor
  if atEnd rest then
    if Array.null parsed.value then
      Left ("Could not parse citation: " <> citation)
    else
      Right parsed.value
  else
    Left ("Could not parse citation: " <> citation)

expandCitation :: Array ChapterSegment -> Array VerseRef
expandCitation segments =
  segments >>= \seg ->
    seg.parts >>= expandPart seg.chapter

expandPart :: Int -> VersePart -> Array VerseRef
expandPart chapter part =
  case part of
    Single v -> [ { chapter, verse: v } ]
    Range start end ->
      let lo = min start end
          hi = max start end
      in Array.range lo hi <#> \v -> { chapter, verse: v }
    CrossChapterRange { startVerse, endChapter, endVerse } ->
      -- Return endpoint markers only; Bible.purs will expand the full range
      [ { chapter, verse: startVerse }
      , { chapter: endChapter, verse: endVerse }
      ]

mkCursor :: String -> Cursor
mkCursor input =
  { chars: toCharArray input
  , pos: 0
  }

atEnd :: Cursor -> Boolean
atEnd cursor =
  cursor.pos >= Array.length cursor.chars

peek :: Cursor -> Maybe Char
peek cursor =
  Array.index cursor.chars cursor.pos

advance :: Cursor -> Cursor
advance cursor =
  cursor { pos = cursor.pos + 1 }

advanceBy :: Int -> Cursor -> Cursor
advanceBy n cursor =
  cursor { pos = cursor.pos + n }

skipSpaces :: Cursor -> Cursor
skipSpaces cursor =
  case peek cursor of
    Just c | isSpaceChar c -> skipSpaces (advance cursor)
    _ -> cursor

parseSegments :: Cursor -> Either String (Parsed (Array ChapterSegment))
parseSegments cursor = do
  first <- parseSegment cursor
  parseMoreSegments [ first.value ] first.cursor

parseMoreSegments :: Array ChapterSegment -> Cursor -> Either String (Parsed (Array ChapterSegment))
parseMoreSegments acc cursor =
  let cursor' = skipSpaces cursor
  in if atEnd cursor' then
       Right { value: acc, cursor: cursor' }
     else if isSegmentSeparator cursor' then do
       let nextCursor = consumeSegmentSeparator cursor'
       next <- parseSegment nextCursor
       parseMoreSegments (acc <> [ next.value ]) next.cursor
     else
       Right { value: acc, cursor: cursor' }

parseSegment :: Cursor -> Either String (Parsed ChapterSegment)
parseSegment cursor = do
  let cursor' = skipSpaces cursor
  chapterParsed <- parseInt cursor'
  colonCursor <- parseColon chapterParsed.cursor
  partsParsed <- parseVerseParts colonCursor
  Right
    { value:
        { chapter: chapterParsed.value
        , parts: partsParsed.value
        }
    , cursor: partsParsed.cursor
    }

parseColon :: Cursor -> Either String Cursor
parseColon cursor =
  let cursor' = skipSpaces cursor
  in case peek cursor' of
    Just ':' -> Right (skipSpaces (advance cursor'))
    _ -> Left "Expected ':'"

parseVerseParts :: Cursor -> Either String (Parsed (Array VersePart))
parseVerseParts cursor = do
  first <- parseVersePart cursor
  parseMoreParts [ first.value ] first.cursor

parseMoreParts :: Array VersePart -> Cursor -> Either String (Parsed (Array VersePart))
parseMoreParts acc cursor =
  let cursor' = skipSpaces cursor
  in if isPartSeparator cursor' then do
       let nextCursor = consumePartSeparator cursor'
       next <- parseVersePart nextCursor
       parseMoreParts (acc <> [ next.value ]) next.cursor
     else
       Right { value: acc, cursor: cursor' }

parseVersePart :: Cursor -> Either String (Parsed VersePart)
parseVersePart cursor = do
  startParsed <- parseInt cursor
  let cursor' = skipSpaces startParsed.cursor
  case peek cursor' of
    Just c | isDashChar c -> do
      -- Advance past the dash
      let afterDash = skipSpaces (advance cursor')
      -- Look ahead to see if this is a cross-chapter range
      let digitsPeek = takeDigits afterDash
      if Array.null digitsPeek.digits then
        Left "Expected number after dash"
      else do
        -- Check if the number is followed by a colon
        let afterNumber = skipSpaces digitsPeek.cursor
        case peek afterNumber of
          Just ':' -> do
            -- Cross-chapter range: parse "3:6"
            endChapterParsed <- parseInt afterDash
            colonCursor <- parseColon endChapterParsed.cursor
            endVerseParsed <- parseInt colonCursor
            Right
              { value: CrossChapterRange
                  { startVerse: startParsed.value
                  , endChapter: endChapterParsed.value
                  , endVerse: endVerseParsed.value
                  }
              , cursor: endVerseParsed.cursor
              }
          _ -> do
            -- Same-chapter range: parse just the ending verse number
            endParsed <- parseInt afterDash
            Right { value: Range startParsed.value endParsed.value, cursor: endParsed.cursor }
    _ ->
      Right { value: Single startParsed.value, cursor: cursor' }

parseInt :: Cursor -> Either String (Parsed Int)
parseInt cursor =
  let cursor' = skipSpaces cursor
      parsed = takeDigits cursor'
  in if Array.null parsed.digits then
       Left "Expected number"
     else case Int.fromString (fromCharArray parsed.digits) of
       Nothing -> Left "Invalid number"
       Just n -> Right { value: n, cursor: parsed.cursor }

takeDigits :: Cursor -> { digits :: Array Char, cursor :: Cursor }
takeDigits cursor =
  go cursor []
  where
  go cur acc =
    case peek cur of
      Just c | isDigitChar c -> go (advance cur) (acc <> [ c ])
      _ -> { digits: acc, cursor: cur }

isSegmentSeparator :: Cursor -> Boolean
isSegmentSeparator cursor =
  case peek cursor of
    Just ';' -> true
    Just ',' -> isCommaSegmentSeparator cursor
    _ -> isAndSegmentSeparator cursor

consumeSegmentSeparator :: Cursor -> Cursor
consumeSegmentSeparator cursor =
  let cursor' = skipSpaces cursor
  in case peek cursor' of
    Just ';' -> skipSpaces (advance cursor')
    Just ',' -> skipSpaces (advance cursor')
    _ | startsWithAnd cursor' -> skipSpaces (advanceBy 3 cursor')
    _ -> cursor'

isPartSeparator :: Cursor -> Boolean
isPartSeparator cursor =
  let cursor' = skipSpaces cursor
  in if isCommaSegmentSeparator cursor' then
       false
     else case peek cursor' of
       Just ',' -> true
       _ -> startsWithAnd cursor' && not (isAndSegmentSeparator cursor')

consumePartSeparator :: Cursor -> Cursor
consumePartSeparator cursor =
  let cursor' = skipSpaces cursor
  in case peek cursor' of
    Just ',' -> skipSpaces (advance cursor')
    _ | startsWithAnd cursor' -> skipSpaces (advanceBy 3 cursor')
    _ -> cursor'

isCommaSegmentSeparator :: Cursor -> Boolean
isCommaSegmentSeparator cursor =
  case peek cursor of
    Just ',' -> isChapterStart (skipSpaces (advance cursor))
    _ -> false

isAndSegmentSeparator :: Cursor -> Boolean
isAndSegmentSeparator cursor =
  if startsWithAnd cursor then
    isChapterStart (skipSpaces (advanceBy 3 cursor))
  else
    false

isChapterStart :: Cursor -> Boolean
isChapterStart cursor =
  let cursor' = skipSpaces cursor
      parsed = takeDigits cursor'
  in if Array.null parsed.digits then
       false
     else
       case peek (skipSpaces parsed.cursor) of
         Just ':' -> true
         _ -> false

startsWithAnd :: Cursor -> Boolean
startsWithAnd cursor =
  let chars = cursor.chars
      pos = cursor.pos
      len = Array.length chars
  in if pos + 2 >= len then
       false
     else case Array.index chars pos, Array.index chars (pos + 1), Array.index chars (pos + 2) of
       Just a, Just n, Just d ->
         let matches =
               toLowerChar a == 'a' && toLowerChar n == 'n' && toLowerChar d == 'd'
             after = Array.index chars (pos + 3)
         in matches && case after of
           Just c | isAlphaChar c -> false
           _ -> true
       _, _, _ -> false

findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex predicate arr =
  go 0
  where
  go idx =
    case Array.index arr idx of
      Nothing -> Nothing
      Just value ->
        if predicate value then
          Just idx
        else
          go (idx + 1)

findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex predicate arr =
  go (Array.length arr - 1)
  where
  go idx =
    if idx < 0 then
      Nothing
    else case Array.index arr idx of
      Nothing -> Nothing
      Just value ->
        if predicate value then
          Just idx
        else
          go (idx - 1)

isDigitChar :: Char -> Boolean
isDigitChar c =
  let code = toCharCode c
  in code >= 48 && code <= 57

isAlphaChar :: Char -> Boolean
isAlphaChar c =
  let code = toCharCode c
  in (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

isSpaceChar :: Char -> Boolean
isSpaceChar c =
  case c of
    ' ' -> true
    '\t' -> true
    '\n' -> true
    '\r' -> true
    _ -> false

toLowerChar :: Char -> Char
toLowerChar c =
  let code = toCharCode c
  in if code >= 65 && code <= 90 then
       case fromCharCode (code + 32) of
         Just lowered -> lowered
         Nothing -> c
     else
       c

isDashChar :: Char -> Boolean
isDashChar c =
  case c of
    '-' -> true  -- ASCII hyphen (U+002D)
    '–' -> true  -- En-dash (U+2013)
    '—' -> true  -- Em-dash (U+2014)
    _ -> false
