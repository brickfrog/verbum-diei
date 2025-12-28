module VerbumDiei.Rss
  ( Feed
  , FeedItem
  , FeedReading
  , parseWordOfDayFeed
  ) where

import Prelude

import Data.Array as Array
import Data.Char as Char
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits as CodeUnits
import Data.String.Common (trim)

type Feed =
  { title :: String
  , link :: String
  , items :: Array FeedItem
  }

type FeedItem =
  { title :: String
  , guid :: String
  , date :: String
  , pubDate :: String
  , descriptionHtml :: String
  , readings :: Array FeedReading
  }

type FeedReading =
  { kind :: String
  , heading :: String
  , book :: String
  , citation :: String
  , bibleApiReference :: String
  }

data XmlToken
  = OpenTag String
  | CloseTag String
  | Text String
  | CData String

data HtmlToken
  = PStart
  | PEnd
  | Break
  | HtmlText String

parseWordOfDayFeed :: String -> Feed
parseWordOfDayFeed xml =
  let
    tokens = tokenizeXml xml
    channelTokens = fromMaybe [] (extractTagBlock "channel" tokens)
    title = firstTagText "title" channelTokens
    link = firstTagText "link" channelTokens
    items =
      extractTagBlocks "item" channelTokens
        <#> parseItem
  in
    { title, link, items }

parseItem :: Array XmlToken -> FeedItem
parseItem tokens =
  let
    title = firstTagText "title" tokens
    guid = firstTagText "guid" tokens
    pubDate = firstTagText "pubdate" tokens
    descriptionHtml = firstTagText "description" tokens
    readings = extractReadingsFromDescription descriptionHtml
  in
    { title
    , guid
    , date: dateFromGuid guid
    , pubDate
    , descriptionHtml
    , readings
    }

firstTagText :: String -> Array XmlToken -> String
firstTagText tag tokens =
  case Array.head (collectTagText tag tokens) of
    Nothing -> ""
    Just raw -> trim (decodeEntities raw)

collectTagText :: String -> Array XmlToken -> Array String
collectTagText tag tokens =
  let
    target = normalizeTagName tag
    go remaining depth current acc =
      case Array.uncons remaining of
        Nothing ->
          Array.reverse acc
        Just { head: t, tail: rest } ->
          case t of
            OpenTag name | name == target ->
              if depth == 0 then
                go rest 1 [] acc
              else
                go rest (depth + 1) current acc
            CloseTag name | name == target ->
              if depth == 1 then
                let
                  value = joinWith "" (Array.reverse current)
                in
                  go rest 0 [] (value `cons` acc)
              else
                go rest (max 0 (depth - 1)) current acc
            Text txt | depth > 0 ->
              go rest depth (txt `cons` current) acc
            CData txt | depth > 0 ->
              go rest depth (txt `cons` current) acc
            _ ->
              go rest depth current acc
  in
    go tokens 0 [] []

extractTagBlock :: String -> Array XmlToken -> Maybe (Array XmlToken)
extractTagBlock tag tokens =
  case Array.head (extractTagBlocks tag tokens) of
    Nothing -> Nothing
    Just block -> Just block

extractTagBlocks :: String -> Array XmlToken -> Array (Array XmlToken)
extractTagBlocks tag tokens =
  let
    target = normalizeTagName tag
    go remaining depth current acc =
      case Array.uncons remaining of
        Nothing ->
          Array.reverse acc
        Just { head: t, tail: rest } ->
          case t of
            OpenTag name | name == target ->
              if depth == 0 then
                go rest 1 [] acc
              else
                go rest (depth + 1) (t `cons` current) acc
            CloseTag name | name == target ->
              if depth == 1 then
                let
                  block = Array.reverse current
                in
                  go rest 0 [] (block `cons` acc)
              else
                go rest (max 0 (depth - 1)) (t `cons` current) acc
            _ ->
              if depth > 0 then
                go rest depth (t `cons` current) acc
              else
                go rest depth current acc
  in
    go tokens 0 [] []

extractReadingsFromDescription :: String -> Array FeedReading
extractReadingsFromDescription descriptionHtml =
  let
    paragraphs = extractParagraphs descriptionHtml
    readings = paragraphs # Array.mapMaybe parseReadingFromParagraph
    firstReading = Array.find (\r -> r.kind == "first") readings
    gospelReading = Array.find (\r -> r.kind == "gospel") readings
  in
    Array.catMaybes [ firstReading, gospelReading ]

parseReadingFromParagraph :: String -> Maybe FeedReading
parseReadingFromParagraph paragraph = do
  let
    lines =
      split (Pattern "\n") paragraph
        # map trim
        # Array.filter (_ /= "")
    citationIndex = Array.findIndex isCitationLine lines
  idx <- citationIndex
  citationLine <- Array.index lines idx
  let
    heading = fromMaybe "" (Array.index lines 0)
    splitResult = splitBookCitation citationLine
    citation = normalizeCitation splitResult.citation
    headingBook = extractBookFromHeading heading
    book = case splitResult.book of
      Just b -> normalizeBookName b
      Nothing -> normalizeBookName headingBook
    kind =
      if isGospelHeading heading || isGospelBook book then
        "gospel"
      else
        "first"
    bibleApiReference = trim (book <> " " <> citation)
  if book == "" || citation == "" then
    Nothing
  else
    Just
      { kind
      , heading
      , book
      , citation
      , bibleApiReference
      }

type SplitCitation =
  { book :: Maybe String
  , citation :: String
  }

splitBookCitation :: String -> SplitCitation
splitBookCitation line =
  let
    tokens = splitWords line
    idx = Array.findIndex tokenHasDigitOrColon tokens
  in
    case idx of
      Nothing ->
        { book: Nothing, citation: "" }
      Just i ->
        let
          bookTokens = Array.take i tokens
          citationTokens = Array.drop i tokens
          citationTokens' =
            takeWhileArray (\t -> toLowerString t /= "or") citationTokens
          book =
            if Array.null bookTokens then
              Nothing
            else
              Just (joinWith " " bookTokens)
          citation = joinWith " " citationTokens'
        in
          { book, citation }

extractParagraphs :: String -> Array String
extractParagraphs html =
  let
    tokens = tokenizeHtml html
    flush current acc =
      let
        text = joinWith "" (Array.reverse current)
        trimmed = trim text
      in
        if trimmed == "" then
          acc
        else
          trimmed `cons` acc
    go remaining current acc =
      case Array.uncons remaining of
        Nothing ->
          Array.reverse (flush current acc)
        Just { head: t, tail: rest } ->
          case t of
            PStart ->
              if Array.null current then
                go rest [] acc
              else
                go rest [] (flush current acc)
            PEnd ->
              go rest [] (flush current acc)
            Break ->
              go rest ("\n" `cons` current) acc
            HtmlText txt ->
              go rest (decodeEntities txt `cons` current) acc
  in
    go tokens [] []

tokenizeHtml :: String -> Array HtmlToken
tokenizeHtml input =
  let
    chars = CodeUnits.toCharArray input
    len = Array.length chars
    go pos acc =
      if pos >= len then
        Array.reverse acc
      else case Array.index chars pos of
        Just '<' ->
          case parseTag chars pos of
            Just tagInfo ->
              let
                nextPos = tagInfo.next
                name = normalizeTagName tagInfo.name
                acc' =
                  if tagInfo.isClosing then
                    if name == "p" then
                      PEnd `cons` acc
                    else
                      acc
                  else if name == "p" then
                    PStart `cons` acc
                  else if name == "br" then
                    Break `cons` acc
                  else
                    acc
              in
                go nextPos acc'
            Nothing ->
              go (pos + 1) acc
        _ ->
          let
            next = fromMaybe len (findCharFrom chars '<' pos)
            chunk = CodeUnits.fromCharArray (Array.slice pos next chars)
          in
            go next (HtmlText chunk `cons` acc)
  in
    go 0 []

tokenizeXml :: String -> Array XmlToken
tokenizeXml input =
  let
    chars = CodeUnits.toCharArray input
    len = Array.length chars
    cdataStart = CodeUnits.toCharArray "<![CDATA["
    cdataEnd = CodeUnits.toCharArray "]]>"
    commentStart = CodeUnits.toCharArray "<!--"
    commentEnd = CodeUnits.toCharArray "-->"
    piStart = CodeUnits.toCharArray "<?"
    piEnd = CodeUnits.toCharArray "?>"
    go pos acc =
      if pos >= len then
        Array.reverse acc
      else case Array.index chars pos of
        Just '<' ->
          if startsWithAt chars pos cdataStart then
            case findSubsequence chars cdataEnd (pos + Array.length cdataStart) of
              Nothing ->
                Array.reverse acc
              Just endPos ->
                let
                  content = Array.slice (pos + Array.length cdataStart) endPos chars
                in
                  go (endPos + Array.length cdataEnd) (CData (CodeUnits.fromCharArray content) `cons` acc)
          else if startsWithAt chars pos commentStart then
            case findSubsequence chars commentEnd (pos + Array.length commentStart) of
              Nothing -> Array.reverse acc
              Just endPos -> go (endPos + Array.length commentEnd) acc
          else if startsWithAt chars pos piStart then
            case findSubsequence chars piEnd (pos + Array.length piStart) of
              Nothing -> Array.reverse acc
              Just endPos -> go (endPos + Array.length piEnd) acc
          else if startsWithAt chars pos (CodeUnits.toCharArray "</") then
            case parseTag chars pos of
              Just tagInfo ->
                go tagInfo.next (CloseTag (normalizeTagName tagInfo.name) `cons` acc)
              Nothing ->
                go (pos + 1) acc
          else if startsWithAt chars pos (CodeUnits.toCharArray "<!") then
            case findCharFrom chars '>' (pos + 2) of
              Nothing -> Array.reverse acc
              Just endPos -> go (endPos + 1) acc
          else
            case parseTag chars pos of
              Just tagInfo ->
                go tagInfo.next (OpenTag (normalizeTagName tagInfo.name) `cons` acc)
              Nothing ->
                go (pos + 1) acc
        _ ->
          let
            next = fromMaybe len (findCharFrom chars '<' pos)
            chunk = CodeUnits.fromCharArray (Array.slice pos next chars)
          in
            go next (Text chunk `cons` acc)
  in
    go 0 []

type TagInfo =
  { name :: String
  , isClosing :: Boolean
  , next :: Int
  }

parseTag :: Array Char -> Int -> Maybe TagInfo
parseTag chars pos = do
  start <- Array.index chars pos
  if start /= '<' then
    Nothing
  else do
    let
      len = Array.length chars
      isClosing =
        case Array.index chars (pos + 1) of
          Just '/' -> true
          _ -> false
      nameStart = if isClosing then pos + 2 else pos + 1
    end <- findTagEnd chars (pos + 1)
    let
      nameChars = takeWhileTagName chars nameStart
      name = CodeUnits.fromCharArray nameChars
      next = end + 1
    if name == "" || next > len then
      Nothing
    else
      Just { name, isClosing, next }

findTagEnd :: Array Char -> Int -> Maybe Int
findTagEnd chars start =
  let
    len = Array.length chars
    go i quote =
      if i >= len then
        Nothing
      else
        case Array.index chars i of
          Just '>' | quote == Nothing -> Just i
          Just '"' ->
            let nextQuote = if quote == Just '"' then Nothing else Just '"'
            in go (i + 1) nextQuote
          Just '\'' ->
            let nextQuote = if quote == Just '\'' then Nothing else Just '\''
            in go (i + 1) nextQuote
          _ -> go (i + 1) quote
  in
    go start Nothing

takeWhileTagName :: Array Char -> Int -> Array Char
takeWhileTagName chars start =
  let
    len = Array.length chars
    go i acc =
      if i >= len then
        Array.reverse acc
      else
        case Array.index chars i of
          Just c | isTagNameChar c -> go (i + 1) (c `cons` acc)
          _ -> Array.reverse acc
  in
    go start []

isTagNameChar :: Char -> Boolean
isTagNameChar c =
  isAlphaChar c || isDigitChar c || c == ':' || c == '-' || c == '_'

startsWithAt :: Array Char -> Int -> Array Char -> Boolean
startsWithAt chars pos needle =
  let
    len = Array.length chars
    nlen = Array.length needle
  in
    if pos + nlen > len then
      false
    else
      Array.slice pos (pos + nlen) chars == needle

findSubsequence :: Array Char -> Array Char -> Int -> Maybe Int
findSubsequence chars needle start =
  let
    len = Array.length chars
    nlen = Array.length needle
    go i =
      if i + nlen > len then
        Nothing
      else if Array.slice i (i + nlen) chars == needle then
        Just i
      else
        go (i + 1)
  in
    go start

findCharFrom :: Array Char -> Char -> Int -> Maybe Int
findCharFrom chars target start =
  let
    len = Array.length chars
    go i =
      if i >= len then
        Nothing
      else case Array.index chars i of
        Just c | c == target -> Just i
        _ -> go (i + 1)
  in
    go start

normalizeTagName :: String -> String
normalizeTagName name =
  let
    lower = toLowerString name
    parts = split (Pattern ":") lower
  in
    case Array.last parts of
      Nothing -> lower
      Just lastPart -> lastPart

decodeEntities :: String -> String
decodeEntities input =
  let
    chars = CodeUnits.toCharArray input
    len = Array.length chars
    go pos acc =
      if pos >= len then
        CodeUnits.fromCharArray (Array.reverse acc)
      else case Array.index chars pos of
        Just '&' ->
          case findCharFrom chars ';' pos of
            Nothing ->
              go (pos + 1) ('&' `cons` acc)
            Just endPos ->
              let
                entity = CodeUnits.fromCharArray (Array.slice (pos + 1) endPos chars)
              in
                case decodeEntity entity of
                  Just replacement ->
                    go (endPos + 1) (replacement `cons` acc)
                  Nothing ->
                    let
                      raw = Array.slice pos (endPos + 1) chars
                    in
                      go (endPos + 1) (Array.reverse raw <> acc)
        Just c ->
          go (pos + 1) (c `cons` acc)
        Nothing ->
          CodeUnits.fromCharArray (Array.reverse acc)
  in
    go 0 []

decodeEntity :: String -> Maybe Char
decodeEntity entity =
  case toLowerString entity of
    "nbsp" -> Just ' '
    "quot" -> Just '"'
    "amp" -> Just '&'
    "lt" -> Just '<'
    "gt" -> Just '>'
    "apos" -> Just '\''
    "#39" -> Just '\''
    _ ->
      if startsWithString "#x" entity then
        parseHex (dropString 2 entity) >>= Char.fromCharCode
      else if startsWithString "#" entity then
        Int.fromString (dropString 1 entity) >>= Char.fromCharCode
      else
        Nothing

startsWithString :: String -> String -> Boolean
startsWithString prefix value =
  let
    p = CodeUnits.toCharArray prefix
    v = CodeUnits.toCharArray value
  in
    startsWithAt v 0 p

dropString :: Int -> String -> String
dropString n s =
  CodeUnits.fromCharArray (Array.drop n (CodeUnits.toCharArray s))

parseHex :: String -> Maybe Int
parseHex s =
  let
    chars = CodeUnits.toCharArray s
    go remaining acc =
      case Array.uncons remaining of
        Nothing -> Just acc
        Just { head: c, tail: rest } ->
          case hexValue c of
            Nothing -> Nothing
            Just v -> go rest (acc * 16 + v)
  in
    go chars 0

hexValue :: Char -> Maybe Int
hexValue c
  | c >= '0' && c <= '9' = Just (Char.toCharCode c - Char.toCharCode '0')
  | c >= 'a' && c <= 'f' = Just (10 + Char.toCharCode c - Char.toCharCode 'a')
  | c >= 'A' && c <= 'F' = Just (10 + Char.toCharCode c - Char.toCharCode 'A')
  | otherwise = Nothing

dateFromGuid :: String -> String
dateFromGuid guid =
  let
    parts = split (Pattern "/") guid
    lastParts = Array.reverse parts
  in
    case Array.uncons lastParts of
      Just { head: dayPart, tail: rest1 } ->
        case Array.uncons rest1 of
          Just { head: month, tail: rest2 } ->
            case Array.uncons rest2 of
              Just { head: year } ->
                case CodeUnits.stripSuffix (Pattern ".html") dayPart of
                  Just day ->
                    if isIsoDateParts year month day then
                      year <> "-" <> month <> "-" <> day
                    else
                      ""
                  Nothing -> ""
              _ -> ""
          _ -> ""
      _ -> ""

isIsoDateParts :: String -> String -> String -> Boolean
isIsoDateParts y m d =
  CodeUnits.length y == 4
    && CodeUnits.length m == 2
    && CodeUnits.length d == 2
    && allDigits y
    && allDigits m
    && allDigits d

allDigits :: String -> Boolean
allDigits =
  CodeUnits.toCharArray >>> Array.all isDigitChar

isCitationLine :: String -> Boolean
isCitationLine line =
  let
    chars = CodeUnits.toCharArray line
    len = Array.length chars
    go i =
      if i >= len then
        false
      else case Array.index chars i of
        Just c | isDigitChar c ->
          let
            j = skipDigits (i + 1)
            k = skipSpaces j
          in
            case Array.index chars k of
              Just ':' ->
                let
                  m = skipSpaces (k + 1)
                in
                  case Array.index chars m of
                    Just d | isDigitChar d -> true
                    _ -> go (i + 1)
              _ -> go (i + 1)
        _ -> go (i + 1)
    skipDigits i =
      case Array.index chars i of
        Just c | isDigitChar c -> skipDigits (i + 1)
        _ -> i
    skipSpaces i =
      case Array.index chars i of
        Just c | isSpaceChar c -> skipSpaces (i + 1)
        _ -> i
  in
    go 0

tokenHasDigitOrColon :: String -> Boolean
tokenHasDigitOrColon token =
  let
    chars = CodeUnits.toCharArray token
  in
    Array.any (\c -> isDigitChar c || c == ':') chars

normalizeCitation :: String -> String
normalizeCitation citation =
  citation
    # stripParens
    # collapseSpaces
    # stripLetterSuffix
    # stripTrailingPunctuation
    # removeSpacesAfterComma
    # trim

stripParens :: String -> String
stripParens s =
  CodeUnits.fromCharArray
    (CodeUnits.toCharArray s # Array.filter (\c -> c /= '(' && c /= ')'))

collapseSpaces :: String -> String
collapseSpaces s =
  let
    chars = CodeUnits.toCharArray s
    go remaining prevSpace acc =
      case Array.uncons remaining of
        Nothing -> CodeUnits.fromCharArray (Array.reverse acc)
        Just { head: c, tail: rest } ->
          if isSpaceChar c then
            if prevSpace then
              go rest true acc
            else
              go rest true (' ' `cons` acc)
          else
            go rest false (c `cons` acc)
  in
    go chars false []

stripTrailingPunctuation :: String -> String
stripTrailingPunctuation s =
  let
    chars = CodeUnits.toCharArray s
    go remaining =
      case Array.last remaining of
        Just c | c == '.' || c == ';' || c == ',' ->
          go (dropEnd 1 remaining)
        _ -> CodeUnits.fromCharArray remaining
  in
    go chars

removeSpacesAfterComma :: String -> String
removeSpacesAfterComma s =
  let
    chars = CodeUnits.toCharArray s
    go remaining acc =
      case Array.uncons remaining of
        Nothing -> CodeUnits.fromCharArray (Array.reverse acc)
        Just { head: c, tail: rest } ->
          if c == ',' then
            let
              rest' = dropWhile isSpaceChar rest
            in
              go rest' (',' `cons` acc)
          else
            go rest (c `cons` acc)
  in
    go chars []

stripLetterSuffix :: String -> String
stripLetterSuffix s =
  let
    chars = CodeUnits.toCharArray s
    go remaining acc =
      case Array.uncons remaining of
        Nothing -> CodeUnits.fromCharArray (Array.reverse acc)
        Just { head: d, tail: rest } ->
          if isDigitChar d then
            case Array.uncons rest of
              Just { head: c, tail: rest' } | isAlphaChar c ->
                go rest' (d `cons` acc)
              _ ->
                go rest (d `cons` acc)
          else
            go rest (d `cons` acc)
  in
    go chars []

splitWords :: String -> Array String
splitWords s =
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
          if isSpaceChar c then
            go rest [] (push acc current)
          else
            go rest (c `cons` current) acc
  in
    go chars [] []

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

extractBookFromHeading :: String -> String
extractBookFromHeading heading =
  let
    tokens = tokenizeWords heading
    has token = Array.elem token tokens
    context =
      if has "letter" then
        Just "letter"
      else if has "book" then
        Just "book"
      else
        Nothing
    bookTokens =
      if has "gospel" then
        preferNonEmpty (afterLast "to" tokens) (afterLast "of" tokens)
      else if has "acts" && has "apostles" then
        [ "acts" ]
      else if has "letter" then
        preferNonEmpty (afterLast "to" tokens) (afterLast "of" tokens)
      else if has "book" || has "reading" then
        afterLast "of" tokens
      else
        afterLast "of" tokens
    ordinal = findOrdinal tokens
    baseBook = normalizeBookTokens bookTokens
    withOrdinal =
      case ordinal of
        Just ord -> ord <> " " <> baseBook
        Nothing -> baseBook
    withDefault =
      case context of
        Just "letter" -> applyDefaultOrdinal defaultOrdinalLetters withOrdinal
        Just "book" -> applyDefaultOrdinal defaultOrdinalBooks withOrdinal
        _ -> withOrdinal
  in
    withDefault

afterLast :: String -> Array String -> Array String
afterLast marker tokens =
  case findLastIndex (_ == marker) tokens of
    Nothing -> []
    Just idx -> Array.drop (idx + 1) tokens

findOrdinal :: Array String -> Maybe String
findOrdinal tokens =
  let
    lookup =
      [ { key: "first", value: "1" }
      , { key: "second", value: "2" }
      , { key: "third", value: "3" }
      , { key: "fourth", value: "4" }
      , { key: "1st", value: "1" }
      , { key: "2nd", value: "2" }
      , { key: "3rd", value: "3" }
      , { key: "4th", value: "4" }
      , { key: "i", value: "1" }
      , { key: "ii", value: "2" }
      , { key: "iii", value: "3" }
      , { key: "iv", value: "4" }
      , { key: "1", value: "1" }
      , { key: "2", value: "2" }
      , { key: "3", value: "3" }
      , { key: "4", value: "4" }
      ]
    match = Array.find (\entry -> Array.elem entry.key tokens) lookup
  in
    match <#> _.value

defaultOrdinalBooks :: Array { key :: String, value :: String }
defaultOrdinalBooks =
  [ { key: "samuel", value: "1 Samuel" }
  , { key: "kings", value: "1 Kings" }
  , { key: "chronicles", value: "1 Chronicles" }
  , { key: "maccabees", value: "1 Maccabees" }
  ]

defaultOrdinalLetters :: Array { key :: String, value :: String }
defaultOrdinalLetters =
  [ { key: "corinthians", value: "1 Corinthians" }
  , { key: "thessalonians", value: "1 Thessalonians" }
  , { key: "timothy", value: "1 Timothy" }
  , { key: "peter", value: "1 Peter" }
  , { key: "john", value: "1 John" }
  ]

applyDefaultOrdinal :: Array { key :: String, value :: String } -> String -> String
applyDefaultOrdinal defaults book =
  let
    key = normalizeBookKey book
  in
    case Array.find (\entry -> entry.key == key) defaults of
      Just entry -> entry.value
      Nothing -> book

normalizeBookTokens :: Array String -> String
normalizeBookTokens tokens =
  tokens
    # Array.filter (not <<< isStopword)
    # map titleCase
    # joinWith " "

normalizeBookName :: String -> String
normalizeBookName raw =
  case lookupAlias (normalizeBookKey raw) of
    Just name -> name
    Nothing ->
      tokenizeWords raw
        # normalizeBookTokens

normalizeBookKey :: String -> String
normalizeBookKey s =
  let
    chars = CodeUnits.toCharArray s
    cleaned = chars # Array.filter (\c -> isAlphaNumChar c)
  in
    CodeUnits.fromCharArray (cleaned <#> toLowerChar)

lookupAlias :: String -> Maybe String
lookupAlias key =
  Array.find (\entry -> entry.key == key) bookAliases <#> _.value

bookAliases :: Array { key :: String, value :: String }
bookAliases =
  [ { key: "mt", value: "Matthew" }
  , { key: "matt", value: "Matthew" }
  , { key: "mk", value: "Mark" }
  , { key: "mrk", value: "Mark" }
  , { key: "lk", value: "Luke" }
  , { key: "luk", value: "Luke" }
  , { key: "jn", value: "John" }
  , { key: "jhn", value: "John" }
  , { key: "joh", value: "John" }
  ]

isGospelHeading :: String -> Boolean
isGospelHeading heading =
  Array.elem "gospel" (tokenizeWords heading)

isGospelBook :: String -> Boolean
isGospelBook book =
  case normalizeBookKey book of
    "matthew" -> true
    "mark" -> true
    "luke" -> true
    "john" -> true
    _ -> false

isStopword :: String -> Boolean
isStopword token =
  case token of
    "the" -> true
    "saint" -> true
    "st" -> true
    "of" -> true
    "to" -> true
    "a" -> true
    "an" -> true
    "from" -> true
    "reading" -> true
    "book" -> true
    "letter" -> true
    "gospel" -> true
    "according" -> true
    "holy" -> true
    _ -> false

titleCase :: String -> String
titleCase s =
  let
    chars = CodeUnits.toCharArray s
  in
    case Array.uncons chars of
      Nothing -> ""
      Just { head, tail } ->
        let
          first = toUpperChar head
          rest = tail <#> toLowerChar
        in
          CodeUnits.fromCharArray ([ first ] <> rest)

toLowerString :: String -> String
toLowerString s =
  CodeUnits.fromCharArray (CodeUnits.toCharArray s <#> toLowerChar)

isAlphaNumChar :: Char -> Boolean
isAlphaNumChar c =
  isAlphaChar c || isDigitChar c

isDigitChar :: Char -> Boolean
isDigitChar c =
  let
    code = Char.toCharCode c
  in
    code >= 48 && code <= 57

isAlphaChar :: Char -> Boolean
isAlphaChar c =
  let
    code = Char.toCharCode c
  in
    (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

toLowerChar :: Char -> Char
toLowerChar c =
  let
    code = Char.toCharCode c
  in
    if code >= 65 && code <= 90 then
      case Char.fromCharCode (code + 32) of
        Just lowered -> lowered
        Nothing -> c
    else
      c

toUpperChar :: Char -> Char
toUpperChar c =
  let
    code = Char.toCharCode c
  in
    if code >= 97 && code <= 122 then
      case Char.fromCharCode (code - 32) of
        Just uppered -> uppered
        Nothing -> c
    else
      c

isSpaceChar :: Char -> Boolean
isSpaceChar c =
  case c of
    ' ' -> true
    '\t' -> true
    '\n' -> true
    '\r' -> true
    _ -> false

cons :: forall a. a -> Array a -> Array a
cons x xs = [ x ] <> xs

takeWhileArray :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhileArray predicate arr =
  let
    len = Array.length arr
    go i =
      if i >= len then
        arr
      else
        case Array.index arr i of
          Just value | predicate value -> go (i + 1)
          _ -> Array.take i arr
  in
    go 0

preferNonEmpty :: forall a. Array a -> Array a -> Array a
preferNonEmpty primary fallback =
  if Array.null primary then
    fallback
  else
    primary

dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile predicate arr =
  let
    len = Array.length arr
    go i =
      if i >= len then
        []
      else
        case Array.index arr i of
          Just value | predicate value -> go (i + 1)
          _ -> Array.drop i arr
  in
    go 0

dropEnd :: forall a. Int -> Array a -> Array a
dropEnd n arr =
  let
    len = Array.length arr
  in
    Array.take (max 0 (len - n)) arr

findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex predicate arr =
  let
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
  in
    go (Array.length arr - 1)
