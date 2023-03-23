module RssFeed.Parse
  ( parseRss,
  )
where

import RssFeed.Types
import Text.XML.Light

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

hasPostBeenRead :: [PostID] -> Maybe String -> Maybe String -> Maybe Bool
hasPostBeenRead readPosts guid title =
  case guid of
    Just g -> Just $ elem (Guid g) readPosts
    Nothing ->
      case title of
        Just t -> Just $ elem (TitleID t) readPosts
        Nothing -> Nothing

parseItem :: [PostID] -> Element -> Maybe Item
parseItem readPosts el =
  let title = getTextFromChild el (unqual "title")
      link = getTextFromChild el (unqual "link")
      description = getTextFromChild el (unqual "description")
      pubDate = getTextFromChild el (unqual "pubDate")
      category = getTextFromChild el (unqual "category")
      guid = getTextFromChild el (unqual "guid")
      hasBeenRead = hasPostBeenRead readPosts guid title
   in Item <$> title <*> link <*> description <*> pure pubDate <*> pure category <*> pure guid <*> hasBeenRead

parseItems :: [PostID] -> [Element] -> [Item]
parseItems readPosts =
  concatMap
    ( \el -> case parseItem readPosts el of
        Nothing -> []
        Just item -> [item]
    )

parseChannel :: [PostID] -> Element -> Maybe Channel
parseChannel readPosts el =
  let title = getTextFromChild el (unqual "title")
      link = getTextFromChild el (unqual "link")
      description = getTextFromChild el (unqual "description")
      items = parseItems readPosts $ findChildren (unqual "item") el
   in Channel <$> title <*> link <*> description <*> Just items

parseDocument :: [PostID] -> Element -> Maybe Channel
parseDocument readPosts el = getRss20Channel el >>= parseChannel readPosts

parseRss :: [PostID] -> String -> Maybe Channel
parseRss readPosts raw = parseXMLDoc raw >>= parseDocument readPosts
