module VerbumDiei.Rss
  ( Feed
  , FeedItem
  , FeedReading
  , parseWordOfDayFeed
  ) where

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

foreign import parseWordOfDayFeed :: String -> Feed
