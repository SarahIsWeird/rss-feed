module Main where

import Control.Exception (catch)
import Control.Lens
import Control.Monad (forM, forM_, when)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Foldable (find, for_)
import qualified Data.Map as Map
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

getChannelFromFeeds :: RssFeed.Feeds -> String -> IO (Maybe RssFeed.Channel)
getChannelFromFeeds feeds feedName = do
  let feed = find (\f -> RssFeed.fName f == feedName) feeds
  case feed of
    Nothing -> do
      putStrLn ("Unknown feed " <> feedName)
      pure Nothing
    Just f -> do
      channel <- fetchChannel f
      case channel of
        Nothing -> do
          putStrLn "Failed to fetch RSS data. Are you connected to the internet?"
          pure Nothing
        Just c -> pure (Just c)

listPosts :: RssFeed.Feeds -> OptParse.FeedName -> OptParse.ShowRead -> IO ()
listPosts feeds (OptParse.FeedName feedName) showRead = do
  channel <- getChannelFromFeeds feeds feedName
  case channel of
    Nothing -> pure ()
    Just c ->
      let items = RssFeed.cItems c
          filteredItems = case showRead of
            OptParse.ShowReadPosts -> RssFeed.cItems c
            OptParse.HideReadPosts ->
              case Map.lookup feedName feeds of
                Nothing -> RssFeed.cItems c
                Just feed -> filter (\item -> RssFeed.getPostId item `notElem` RssFeed.fReadPosts feed) (RssFeed.cItems c)
       in for_ filteredItems printItem

appendReadPost :: RssFeed.PostID -> RssFeed.Feed -> RssFeed.Feed
appendReadPost postId (RssFeed.Feed name url readPosts) =
  RssFeed.Feed name url (postId : readPosts)

markReadInFeeds :: RssFeed.Feeds -> String -> RssFeed.PostID -> RssFeed.Feeds
markReadInFeeds feeds feedName postID =
  Map.adjust (appendReadPost postID) feedName feeds

markRead :: RssFeed.Feeds -> OptParse.FeedName -> OptParse.PostName -> IO ()
markRead feeds (OptParse.FeedName feedName) (OptParse.PostName postName) = do
  channel <- getChannelFromFeeds feeds feedName
  case channel of
    Nothing -> pure ()
    Just c ->
      let post = find (\p -> RssFeed.iTitle p == postName) (RssFeed.cItems c)
          postId = fmap RssFeed.getPostId post
       in case postId of
            Nothing -> putStrLn $ "No post with the title \"" <> postName <> "\"!"
            Just id -> BL.writeFile "feeds.json" $ encode $ markReadInFeeds feeds feedName id

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
        OptParse.ListPosts feedName showRead -> listPosts f feedName showRead
        OptParse.MarkRead feedName postName -> markRead f feedName postName
