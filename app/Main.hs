module Main where

import Control.Exception (catch)
import Control.Lens
import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Foldable (for_)
import Network.Wreq
import RssFeed (readFeedConfig)
import qualified RssFeed
import System.Environment (getArgs)
import System.Exit (exitFailure)

printChannel :: RssFeed.Channel -> IO ()
printChannel (RssFeed.Channel title url description items) = do
  putStrLn (title <> " (" <> url <> ")")
  putStrLn description
  putStrLn ""
  forM_ items printItem

putStrLnPrefix :: String -> String -> IO ()
putStrLnPrefix prefix value =
  putStrLn (prefix <> value)

printItem :: RssFeed.Item -> IO ()
printItem (RssFeed.Item title url description pubDate category) = do
  putStrLn title
  putStrLnPrefix "Link: " url
  for_ pubDate (putStrLnPrefix "Date: ")
  for_ category (putStrLnPrefix "Category: ")
  putStrLn ""

fetchAndPrintFeedInfo :: RssFeed.Feed -> IO ()
fetchAndPrintFeedInfo (RssFeed.Feed _ url) = do
  r <- get url
  case r ^. (responseStatus . statusCode) of
    200 ->
      let rss = r ^. responseBody
          result = RssFeed.parseRss (BLU.toString rss)
       in case result of
            Nothing -> putStrLn ("Failed to parse RSS feed of " <> url)
            Just channel -> printChannel channel
    _ -> putStrLn "Failed"

main :: IO ()
main = do
  feeds <- readFeedConfig "feeds.json"
  case feeds of
    Left e -> do
      putStrLn e
      exitFailure
    Right feeds -> mapM_ fetchAndPrintFeedInfo feeds
