{-# LANGUAGE OverloadedStrings #-}

module RssFeed.Types where

import Control.Applicative (empty)
import Data.Aeson
import qualified Data.Text as T

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
    iCategory :: Maybe String
  }

data Feed = Feed
  { fName :: String,
    fUrl :: String
  }
  deriving (Show)

type Feeds = [Feed]

instance FromJSON Feed where
  parseJSON (Object v) =
    let name = v .: "name"
        url = v .: "url"
     in Feed <$> (T.unpack <$> name) <*> (T.unpack <$> url)
  parseJSON _ = empty
