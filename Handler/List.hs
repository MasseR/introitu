module Handler.List where

import Import
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

getListR :: Handler RepHtml
getListR = do
  user <- requireAuth
  notes <- (zip [1 :: Int ..]) <$> (runDB $ selectList [NoteOwner ==. entityKey user] [Asc NoteTopic])
  defaultLayout $ do
    $(widgetFile "list")
  where
    spoiler (Textarea text) = maybe "" id $ listToMaybe $ T.lines text
