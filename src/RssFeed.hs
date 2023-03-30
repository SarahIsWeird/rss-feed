module RssFeed
  ( Channel (..),
    Item (..),
    Feeds,
    Feed (..),
    Url,
    parseRss,
    readFeedConfig,
    getPostId,
  )
where

import RssFeed.Feed
import RssFeed.Parse
import RssFeed.Types
