module Main where

import Control.Exception (catch)
import Control.Monad (when)
import qualified RssFeed
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  rss <- getContents
  let channel = RssFeed.parseRss rss
  case channel of
    Nothing -> putStrLn "Couldn't read file."
    Just (RssFeed.Channel title url description) -> do
      putStrLn $ "Title: " <> title
      putStrLn $ "Link: " <> url
      putStrLn $ "Description: " <> description
