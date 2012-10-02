module Handler.Search where

import Import
import Text.HyperEstraier
import Data.Conduit.Pool
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Text as T
import Handler.List (paginateSelect)
import Control.Arrow ((***))
import Data.List (last)

searchNote :: Int -> UserId -> Text -> Handler (Int, [Entity Note])
searchNote pageNumber uid cond = do
  i <- indexPool <$> getYesod
  runDB $ withResource i (searchNote' pageNumber uid cond)

searchNote' :: Int -> UserId -> Text -> Database -> YesodDB sub App (Int, [Entity Note])
searchNote' pageNumber userId strcond db = do
  foundNoteKeys <- liftIO $ do
    condition <- newCondition
    setPhrase condition strcond
    docids <- searchDatabase db condition
    catMaybes <$> mapM getKey docids
  getNote foundNoteKeys
  where
        getNote noteIds = paginateSelect 15 pageNumber [NoteOwner ==. userId, NoteId <-. noteIds] []
        getKey :: DocumentID -> IO (Maybe NoteId)
        getKey docid = fmap (read . T.unpack) <$> getDocAttr db docid "@key"

getSearchR :: Handler RepHtml
getSearchR = do
  pageNum <- maybe 1 id <$> (runInputGet $ iopt intField "page")
  (Entity userId _) <- requireAuth
  strcond <- runInputGet $ ireq textField "search"
  (pages, notes) <- ((enumFromTo 1) *** (zip [1 :: Int ..])) <$> searchNote pageNum userId strcond
  defaultLayout $ do
    let title = "Search results" :: Html
    setTitle title
    $(widgetFile "list")
  where
    spoiler (Textarea text) = maybe "" id $ listToMaybe $ T.lines text
