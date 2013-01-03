module Handler.LinkInfo where

import Import
import Network.URI (parseURI)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import Data.Maybe (isJust)
import Data.Foldable (msum)

getLinkInfoR :: Handler RepJson
getLinkInfoR = do
  url <- runInputGet $ (msum <$> uriForm)
  case url of
       Just url' -> do
         tags <- parseTags <$> simpleHttp url'
         jsonToRepJson $ object [
             ("summary", paragraphs 3 tags)
           , ("title", title tags)]
       Nothing -> error "Not a valid url"
  where
    uriForm = uris <$> ireq textField "url"
    uris uri = map (fmap show . parseURI . T.unpack) [uri, "http://" `T.append` uri]
    title = innerText . takeWhile (not . isTagClose) . dropWhile (~/= TagOpen ("title" :: String) [])
    textContent = takeWhile (~/= TagClose ("p" :: String)) .
      dropWhile (~/= TagOpen ("p" :: String) [])
    paragraphs n = innerText . take n . textContent
