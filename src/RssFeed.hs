module RssFeed
  ( Channel (..),
    Url,
    parseRss,
  )
where

import Control.Applicative (liftA3)
import Text.XML.Light

type Url = String

data Channel = Channel
  { cTitle :: String,
    cLink :: Url,
    cDescription :: String
  }

isRss20 :: Element -> Bool
isRss20 el =
  let versionAttr = findAttr (unqual "version") el
   in case versionAttr of
        Nothing -> False
        Just version -> version == "2.0"

getRss20Channel :: Element -> Maybe Element
getRss20Channel el =
  if not $ isRss20 el
    then Nothing
    else findChild (unqual "channel") el

extractText :: [Content] -> Maybe String
extractText content =
  case content of
    [Text text] -> Just $ cdData text
    _ -> Nothing

getTextFromChild :: Element -> QName -> Maybe String
getTextFromChild el name =
  findChild name el >>= extractText . elContent

parseChannel :: Element -> Maybe Channel
parseChannel el =
  let title = getTextFromChild el (unqual "title")
      link = getTextFromChild el (unqual "link")
      description = getTextFromChild el (unqual "description")
   in liftA3 Channel title link description

parseDocument :: Element -> Maybe Channel
parseDocument el = getRss20Channel el >>= parseChannel

parseRss :: String -> Maybe Channel
parseRss raw = parseXMLDoc raw >>= parseDocument
