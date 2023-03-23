module Main where

import Control.Exception (catch)
import Control.Lens
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Foldable (find, for_)
import Network.Wreq
import qualified OptParse
import RssFeed (readFeedConfig)
import qualified RssFeed
import qualified RssFeed.Types as RssFeed
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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
printItem (RssFeed.Item title url description pubDate category guid hasBeenRead) = do
  putStr title
  if hasBeenRead
    then putStrLn " (Read)"
    else putStrLn ""
  putStrLnPrefix "Link: " url
  for_ pubDate (putStrLnPrefix "Date: ")
  for_ category (putStrLnPrefix "Category: ")
  putStrLn ""

fetchChannel :: RssFeed.Feed -> IO (Maybe RssFeed.Channel)
fetchChannel (RssFeed.Feed _ url readPosts) = do
  r <- get url
  case r ^. (responseStatus . statusCode) of
    200 ->
      let rss = r ^. responseBody
          result = RssFeed.parseRss readPosts (BLU.toString rss)
       in case result of
            Nothing -> do
              putStrLn ("Failed to parse RSS feed of " <> url)
              pure Nothing
            Just channel -> pure $ Just channel
    _ -> do
      putStrLn "Failed"
      pure Nothing

listFeeds :: RssFeed.Feeds -> IO ()
listFeeds feeds = forM_ feeds $ \feed ->
  putStrLn (RssFeed.fName feed <> " (" <> RssFeed.fUrl feed <> ")")

listPosts :: RssFeed.Feeds -> OptParse.FeedName -> IO ()
listPosts feeds (OptParse.FeedName feedName) = do
  let feed = find (\f -> RssFeed.fName f == feedName) feeds
  case feed of
    Nothing -> putStrLn ("Unknown feed " <> feedName)
    Just f -> do
      channel <- fetchChannel f
      case channel of
        Nothing -> putStrLn "Failed to fetch RSS data. Are you connected to the internet?"
        Just c -> forM_ (RssFeed.cItems c) printItem

main :: IO ()
main = do
  feeds <- readFeedConfig "feeds.json"
  case feeds of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right f -> do
      options <- OptParse.parse
      case options of
        OptParse.ListFeeds -> listFeeds f
        OptParse.ListPosts feedName -> listPosts f feedName
