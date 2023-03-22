module Main where

import Control.Exception (catch)
import Control.Lens
import Control.Monad (when)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Foldable (for_)
import Network.Wreq
import RssFeed (readFeedConfig)
import qualified RssFeed
import System.Environment (getArgs)
import System.Exit (exitFailure)

printChannel :: RssFeed.Channel -> IO ()
printChannel (RssFeed.Channel title url description _) = do
  putStrLn (title <> " (" <> url <> ")")
  putStrLn description
  putStrLn ""

printItem :: RssFeed.Item -> IO ()
printItem (RssFeed.Item title url description) = do
  putStrLn ""
  putStrLn (title <> " (" <> url <> ")")
  putStrLn description

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
  feeds <- readFeedConfig "/Users/sarah/Programming/Haskell/rss-feed/feeds.json"
  case feeds of
    Left e -> do
      putStrLn e
      exitFailure
    Right feeds -> mapM_ fetchAndPrintFeedInfo feeds
  rss <- getContents
  let channel = RssFeed.parseRss rss
  case channel of
    Nothing -> putStrLn "Couldn't read file."
    Just channel -> do
      printChannel channel
      for_ (RssFeed.cItems channel) printItem
