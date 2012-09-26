{-# Language OverloadedStrings #-}
module Model.Note (indexNote) where

import Text.HyperEstraier
import Import
import Data.Text (pack, unpack, append)
import Network.URI
import Data.Conduit.Pool

indexNote :: NoteId -> UTCTime -> (Route App -> Text) -> Handler ()
indexNote noteId time urlRenderer = do
  i <- indexPool <$> getYesod
  runDB $ withResource i (indexNote' noteId time urlRenderer)

indexNote' :: NoteId -> UTCTime -> (Route App -> Text) -> Database -> YesodDB sub App ()
indexNote' noteId time urlRenderer db = do
  note <- get404 noteId
  liftIO $ deleteOld
  liftIO $ indexNew note
  where
    deleteOld = do
      cond <- newCondition
      addAttrCond cond $ append "@key STREQ " (pack $ show noteId)
      docids <- searchDatabase db cond
      mapM_ (\docid -> removeDocument db docid [CleaningRemove]) docids
    indexNew note = do
      doc <- newDocument
      setAttribute doc "@title" (Just $ noteTitle note)
      setAttribute doc "@topic" (Just $ noteTopic note)
      setAttribute doc "@edited" (Just $ pack $ show time)
      setAttribute doc "@key" (Just $ pack $ show noteId)
      setURI doc $ parseURI $ unpack $ urlRenderer (ViewR noteId)
      addText doc (unTextarea $ noteText note)
      putDocument db doc []
