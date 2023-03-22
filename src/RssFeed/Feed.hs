module RssFeed.Feed where

import Control.Exception (Exception (displayException), SomeException (SomeException), catch)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import RssFeed.Types (Feeds)

readFeedConfig :: FilePath -> IO (Either String Feeds)
readFeedConfig path = do
  contents <- catch (fmap Right (BL.readFile path)) (\(SomeException e) -> pure . Left . displayException $ e)
  case contents of
    Left e -> pure (Left e)
    Right c ->
      let decoded = decode c :: Maybe Feeds
       in case decoded of
            Nothing -> pure (Left "Failed to parse feed config.")
            Just feeds -> pure (Right feeds)
