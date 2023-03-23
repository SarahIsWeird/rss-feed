module OptParse
  ( Options (..),
    FeedName (..),
    parse,
  )
where

import Options.Applicative

data Options
  = ListFeeds
  | ListPosts FeedName
  deriving (Show)

newtype FeedName
  = FeedName String
  deriving (Show)

pFeedName :: Parser FeedName
pFeedName = FeedName <$> parser
  where
    parser =
      strOption
        ( long "feed"
            <> short 'f'
            <> metavar "NAME"
            <> help "Feed name"
        )

pListFeeds :: Parser Options
pListFeeds = pure ListFeeds

pListPosts :: Parser Options
pListPosts = ListPosts <$> pFeedName

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
