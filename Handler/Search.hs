module Handler.Search where

import Import
import Text.HyperEstraier
import Data.Conduit.Pool
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Text as T

searchNote :: UserId -> Text -> Handler [Entity Note]
searchNote uid cond = do
  i <- indexPool <$> getYesod
  runDB $ withResource i (searchNote' uid cond)

searchNote' :: UserId -> Text -> Database -> YesodDB sub App [Entity Note]
searchNote' userId strcond db = do
  foundNoteKeys <- liftIO $ do
    condition <- newCondition
    setPhrase condition strcond
    docids <- searchDatabase db condition
    catMaybes <$> mapM getKey docids
  concat <$> mapM getNote foundNoteKeys
  where 
        getNote noteId = selectList [NoteOwner ==. userId, NoteId ==. noteId] []
        getKey :: DocumentID -> IO (Maybe NoteId)
        getKey docid = fmap (read . T.unpack) <$> getDocAttr db docid "@key"

getSearchR :: Handler RepHtml
getSearchR = do
  (Entity userId _) <- requireAuth
  strcond <- runInputGet $ ireq textField "search"
  notes <- (zip [1 :: Int ..]) <$> searchNote userId strcond
  defaultLayout $ do
    let title = "Search results" :: Html
    setTitle title
    $(widgetFile "list")
  where
    spoiler (Textarea text) = maybe "" id $ listToMaybe $ T.lines text
