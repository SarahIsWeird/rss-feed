{-# LANGUAGE OverloadedStrings #-}

module RssFeed.Types where

import Control.Applicative (empty)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
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

getPostId :: Item -> PostID
getPostId item =
  case iGuid item of
    Nothing -> TitleID $ iTitle item
    Just guid -> Guid guid

instance FromJSON PostID where
  parseJSON = withObject "PostID" $ \obj -> do
    title <- obj .:? "title"
    case title of
      Just t -> return (TitleID t)
      Nothing -> fmap Guid (obj .: "guid")

instance ToJSON PostID where
  toJSON postId =
    case postId of
      TitleID titleId -> object ["title" .= titleId]
      Guid guid -> object ["guid" .= guid]

  toEncoding postId =
    case postId of
      TitleID titleId -> pairs ("title" .= titleId)
      Guid guid -> pairs ("guid" .= guid)

data Feed = Feed
  { fName :: String,
    fUrl :: String,
    fReadPosts :: [PostID]
  }
  deriving (Show)

instance FromJSON Feed where
  parseJSON (Object v) =
    let name = v .: "name"
        url = v .: "url"
        readPosts = v .: "read_posts"
     in Feed <$> (T.unpack <$> name) <*> (T.unpack <$> url) <*> readPosts
  parseJSON _ = empty

instance ToJSON Feed where
  toJSON (Feed name url readPosts) =
    object ["name" .= name, "url" .= url, "read_posts" .= readPosts]

  toEncoding (Feed name url readPosts) =
    pairs ("name" .= name <> "url" .= url <> "read_posts" .= readPosts)

type Feeds = Map String Feed
