module OptParse
  ( Options (..),
    FeedName (..),
    PostName (..),
    ShowRead (..),
    parse,
  )
where

import Options.Applicative

data Options
  = ListFeeds
  | ListPosts FeedName ShowRead
  | MarkRead FeedName PostName
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

pListFeeds :: Parser Options
pListFeeds = pure ListFeeds

pListPosts :: Parser Options
pListPosts = ListPosts <$> pFeedName <*> pShowRead

pMarkRead :: Parser Options
pMarkRead = MarkRead <$> pFeedName <*> pPostName

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
