module RssFeed.Feed where

import Control.Exception (Exception (displayException), SomeException (SomeException), catch)
import Control.Monad
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import RssFeed.Types (Feeds)
import System.Directory

readFeedConfig :: FilePath -> IO (Either String Feeds)
readFeedConfig path = do
  configExists <- doesFileExist "feeds.json"
  -- Look. I'm not in the mood to deal with ByteStrings right now, and I don't get them really either. :D
  unless configExists $ writeFile "feeds.json" "{}"
  contents <-
    catch
      (fmap Right (BL.readFile path))
      (\(SomeException e) -> pure . Left . displayException $ e)
  case contents of
    Left e -> pure (Left e)
    Right c ->
      let decoded = decode c :: Maybe Feeds
       in case decoded of
            Nothing -> pure (Left "Failed to parse feed config.")
            Just feeds -> pure (Right feeds)
