module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import VerbumDiei.Bible (fetchBibleReading)
import VerbumDiei.Rss (parseWordOfDayFeed)

main :: Effect Unit
main = do
  launchAff_ do
    test "parses 'and' as a verse separator" do
      reading <- fetchBibleReading "John 20:1 and 2-8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses comma-separated verses" do
      reading <- fetchBibleReading "John 20:1, 2-8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses comma-separated chapters" do
      reading <- fetchBibleReading "John 20:1,21:1-2"
      assertEqual "lineRefs" [ "20:1", "21:1", "21:2" ] reading.lineRefs

    test "keeps comma-separated cross-chapter citations separate" do
      reading <- fetchBibleReading "John 20:1,21:2"
      assertEqual "lineRefs" [ "20:1", "21:2" ] reading.lineRefs
      assertEqual "lines length" 2 (Array.length reading.lines)

    test "keeps semicolon-separated cross-chapter citations separate" do
      reading <- fetchBibleReading "John 20:1;21:2"
      assertEqual "lineRefs" [ "20:1", "21:2" ] reading.lineRefs
      assertEqual "lines length" 2 (Array.length reading.lines)

    test "parses ordinal book references" do
      reading <- fetchBibleReading "1 John 1:1-4"
      assertEqual "lineRefs" [ "1", "2", "3", "4" ] reading.lineRefs

    test "resolves book aliases" do
      reading <- fetchBibleReading "Ecclesiasticus 3:2-3"
      assertEqual "reference" "Sirach 3:2-3" reading.reference
      assertEqual "lineRefs" [ "2", "3" ] reading.lineRefs
      songs <- fetchBibleReading "Songs 3:1-4"
      assertEqual "reference" "Song of Songs 3:1-4" songs.reference
      assertEqual "lineRefs" [ "1", "2", "3", "4" ] songs.lineRefs

    test "resolves book abbreviations" do
      reading <- fetchBibleReading "Jn 3:16"
      assertEqual "reference" "John 3:16" reading.reference
      assertEqual "lineRefs" [ "16" ] reading.lineRefs

    test "tolerates null verses in data" do
      reading <- fetchBibleReading "Exodus 1:18"
      assertEqual "lineRefs" [ "18" ] reading.lineRefs
      assertEqual "lines length" 1 (Array.length reading.lines)

    test "parses en-dash in same-chapter range" do
      reading <- fetchBibleReading "John 20:1–8"
      assertEqual "lineRefs" expectedJohn20 reading.lineRefs

    test "parses cross-chapter range with en-dash" do
      reading <- fetchBibleReading "1 John 2:29–3:6"
      assertEqual "lineRefs" [ "2:29", "3:1", "3:2", "3:3", "3:4", "3:5", "3:6" ] reading.lineRefs
      assertEqual "lines length" 7 (Array.length reading.lines)

    test "repairs an upstream-mangled cross-chapter dash from the feed" do
      let feed = parseWordOfDayFeed mangledDashFeedXml
      item <- case Array.head feed.items of
        Nothing -> throwError (error "expected one feed item")
        Just it -> pure it
      first <- case Array.find (\r -> r.kind == "first") item.readings of
        Nothing -> throwError (error "expected a first reading")
        Just r -> pure r
      assertEqual "first reference" "Habakkuk 1:12-2:4" first.bibleApiReference
      -- An untouched ASCII citation in the same item must survive unchanged.
      gospel <- case Array.find (\r -> r.kind == "gospel") item.readings of
        Nothing -> throwError (error "expected a gospel reading")
        Just r -> pure r
      assertEqual "gospel reference" "Matthew 17:14-20" gospel.bibleApiReference
      reading <- fetchBibleReading first.bibleApiReference
      assertEqual "lineRefs" expectedHabakkuk reading.lineRefs
      assertEqual "lines length" 10 (Array.length reading.lines)

    test "drops multi-letter and upper-case verse-part markers" do
      -- Reaches the parser directly, bypassing the RSS normaliser, so the
      -- citation grammar has to handle the markers on its own.
      reading <- fetchBibleReading "Revelation 11:19A; 12:1-6A, 10AB"
      assertEqual "lineRefs" expectedRevelation reading.lineRefs
      assertEqual "lines length" 8 (Array.length reading.lines)
      lower <- fetchBibleReading "Revelation 11:19a; 12:1-6a, 10ab"
      assertEqual "case agnostic" reading.lineRefs lower.lineRefs

    test "strips verse-part markers from a feed citation" do
      let feed = parseWordOfDayFeed versePartFeedXml
      item <- case Array.head feed.items of
        Nothing -> throwError (error "expected one feed item")
        Just it -> pure it
      first <- case Array.find (\r -> r.kind == "first") item.readings of
        Nothing -> throwError (error "expected a first reading")
        Just r -> pure r
      assertEqual "first reference" "Revelation 11:19; 12:1-6,10" first.bibleApiReference
      reading <- fetchBibleReading first.bibleApiReference
      assertEqual "lineRefs" expectedRevelation reading.lineRefs

    test "resolves collapsed Amos 9:15 verse" do
      reading <- fetchBibleReading "Amos 9:11-15"
      assertEqual "lineRefs" [ "11", "12", "13", "14", "15" ] reading.lineRefs
      assertEqual "lines length" 5 (Array.length reading.lines)

    test "maps 2 Thessalonians 2 across the dropped verse marker" do
      reading <- fetchBibleReading "2 Thessalonians 2:1-3,14-17"
      assertEqual "lineRefs" [ "1", "2", "3", "14", "15", "16", "17" ] reading.lineRefs
      assertEqual "lines length" 7 (Array.length reading.lines)
      -- Modern 2:17 is DRA 2:16. Asserting the text, not just that something
      -- resolved: an off-by-one in the offset would quietly serve 2:16 instead.
      assertEqual "text of modern 2:17"
        (Just "Exhort your hearts and confirm you in every good work and word.")
        (Array.last reading.lines)
      -- Verses before the drop are untouched.
      early <- fetchBibleReading "2 Thessalonians 2:5"
      assertEqual "text of modern 2:5"
        (Just "Remember you not that, when I was yet with you, I told you these things?")
        (Array.head early.lines)


    liftEffect (log "All tests passed.")

test :: String -> Aff Unit -> Aff Unit
test label action = do
  result <- attempt action
  case result of
    Left err -> throwError (error (label <> " failed: " <> show err))
    Right _ -> liftEffect (log ("ok " <> label))

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Aff Unit
assertEqual label expected actual =
  if expected == actual then
    pure unit
  else
    throwError
      (error (label <> " expected " <> show expected <> " but got " <> show actual))

expectedJohn20 :: Array String
expectedJohn20 =
  [ "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  ]

expectedHabakkuk :: Array String
expectedHabakkuk =
  [ "1:12"
  , "1:13"
  , "1:14"
  , "1:15"
  , "1:16"
  , "1:17"
  , "2:1"
  , "2:2"
  , "2:3"
  , "2:4"
  ]

expectedRevelation :: Array String
expectedRevelation =
  [ "11:19"
  , "12:1"
  , "12:2"
  , "12:3"
  , "12:4"
  , "12:5"
  , "12:6"
  , "12:10"
  ]

-- | Trimmed copy of the 2026-08-08 Vatican News item that broke the nightly
-- | run: upstream flattened every non-ASCII character to '?', so the
-- | cross-chapter dash in "1:12-2:4" and the opening quote in the Gospel prose
-- | both arrive as '?'. Only the former may be repaired.
mangledDashFeedXml :: String
mangledDashFeedXml =
  """<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>Word of the day</title>
    <link>https://www.vaticannews.va/en/word-of-the-day.html</link>
    <item>
      <title>Gospel and Word of the Day - 08 August 2026</title>
      <guid>https://www.vaticannews.va/en/word-of-the-day/2026/08/08.html</guid>
      <pubDate>Sat, 08 Aug 2026 00:00:00 +0200</pubDate>
      <description><![CDATA[<p>A reading from the Book of&nbsp;Habakkuk<br /> 1:12?2:4</p>
<p>Are you not from eternity, O LORD,<br /> my holy God, immortal?</p>
<p>From the Gospel according to Matthew<br /> 17:14-20</p>
<p>A man came up to Jesus, knelt down before him, and said,<br /> ?Lord, have pity on my son.</p>]]></description>
    </item>
  </channel>
</rss>"""

-- | Trimmed copy of the 2026-08-15 Vatican News item (the Assumption) that
-- | broke the nightly run: the lectionary cites parts of verses, and upstream
-- | upper-cases the part markers -- "11:19A; 12:1-6A, 10AB". "10AB" is the
-- | awkward one, being a two-letter marker.
versePartFeedXml :: String
versePartFeedXml =
  """<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>Word of the day</title>
    <link>https://www.vaticannews.va/en/word-of-the-day.html</link>
    <item>
      <title>Gospel and Word of the Day - 15 August 2026</title>
      <guid>https://www.vaticannews.va/en/word-of-the-day/2026/08/15.html</guid>
      <pubDate>Sat, 15 Aug 2026 00:00:00 +0200</pubDate>
      <description><![CDATA[<p>A reading from the Book of Revelation<br /> 11:19A; 12:1-6A, 10AB</p>
<p>God?s temple in heaven was opened.</p>
<p>From the Gospel according to Luke<br /> 1:39-56</p>
<p>Mary set out and travelled to the hill country in haste.</p>]]></description>
    </item>
  </channel>
</rss>"""
