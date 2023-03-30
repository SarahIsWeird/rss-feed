module OptParse
  ( Options (..),
    FeedName (..),
    PostName (..),
    ShowRead (..),
    FeedUrl (..),
    parse,
  )
where

import Options.Applicative

data Options
  = ListFeeds
  | ListPosts FeedName ShowRead
  | MarkRead FeedName PostName
  | AddFeed FeedName FeedUrl
  | RemoveFeed FeedName
  deriving (Show)

newtype FeedName
  = FeedName String
  deriving (Show)

newtype PostName
  = PostName String
  deriving (Show)

data ShowRead
  = HideReadPosts
  | ShowReadPosts
  deriving (Show)

newtype FeedUrl
  = FeedUrl String
  deriving (Show)

pFeedName :: Parser FeedName
pFeedName = FeedName <$> parser
  where
    parser =
      argument
        str
        ( metavar "<feed>"
            <> help "Feed name"
        )

pPostName :: Parser PostName
pPostName = PostName <$> parser
  where
    parser =
      argument
        str
        ( metavar "<post>"
            <> help "Post name"
        )

pShowRead :: Parser ShowRead
pShowRead =
  flag
    HideReadPosts
    ShowReadPosts
    ( long "show-read"
        <> short 'r'
        <> help "Show read posts"
    )

pFeedUrl :: Parser FeedUrl
pFeedUrl = FeedUrl <$> parser
  where
    parser =
      argument
        str
        ( metavar "<url>"
            <> help "Feed URL"
        )

pListFeeds :: Parser Options
pListFeeds = pure ListFeeds

pListPosts :: Parser Options
pListPosts = ListPosts <$> pFeedName <*> pShowRead

pMarkRead :: Parser Options
pMarkRead = MarkRead <$> pFeedName <*> pPostName

pAddFeed :: Parser Options
pAddFeed = AddFeed <$> pFeedName <*> pFeedUrl

pRemoveFeed :: Parser Options
pRemoveFeed = RemoveFeed <$> pFeedName

pOptions :: Parser Options
pOptions =
  subparser
    ( command
        "feeds"
        ( info
            (helper <*> pListFeeds)
            (progDesc "List all known feeds")
        )
        <> command
          "posts"
          ( info
              (helper <*> pListPosts)
              (progDesc "List all posts from a feed")
          )
        <> command
          "mark-read"
          ( info
              (helper <*> pMarkRead)
              (progDesc "Mark a post as read")
          )
        <> command
          "add"
          ( info
              (helper <*> pAddFeed)
              (progDesc "Add an RSS feed to your feeds")
          )
        <> command
          "remove"
          ( info
              (helper <*> pRemoveFeed)
              (progDesc "Remove an RSS feed from your feeds")
          )
    )

opts :: ParserInfo Options
opts =
  info
    (pOptions <**> helper)
    ( fullDesc
        <> header "rss-feed - a simple RSS reader"
        <> progDesc "Read your RSS feeds!"
    )

parse :: IO Options
parse = execParser opts
