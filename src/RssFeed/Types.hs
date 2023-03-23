{-# LANGUAGE OverloadedStrings #-}

module RssFeed.Types where

import Control.Applicative (empty)
import Data.Aeson
import qualified Data.Text as T

-- XML data

type Url = String

data Channel = Channel
  { cTitle :: String,
    cLink :: Url,
    cDescription :: String,
    cItems :: [Item]
  }

data Item = Item
  { iTitle :: String,
    iLink :: Url,
    iDescription :: String,
    iPubDate :: Maybe String,
    iCategory :: Maybe String,
    iGuid :: Maybe String,
    iRead :: Bool
  }

-- Parsed data

data PostID
  = TitleID String
  | Guid String
  deriving (Show, Eq)

instance FromJSON PostID where
  parseJSON = withObject "PostID" $ \obj -> do
    title <- obj .:? "title"
    case title of
      Just t -> return (TitleID t)
      Nothing -> fmap Guid (obj .: "guid")

data Feed = Feed
  { fName :: String,
    fUrl :: String,
    fReadPosts :: [PostID]
  }
  deriving (Show)

type Feeds = [Feed]

instance FromJSON Feed where
  parseJSON (Object v) =
    let name = v .: "name"
        url = v .: "url"
        readPosts = v .: "read_posts"
     in Feed <$> (T.unpack <$> name) <*> (T.unpack <$> url) <*> readPosts
  parseJSON _ = empty
