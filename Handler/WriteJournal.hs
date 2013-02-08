module Handler.WriteJournal where

import Import

data JournalForm = JournalForm {
  }

getWriteJournalR :: JournalId -> Handler RepHtml
getWriteJournalR journalId = do
  journal <- runDB $ get404 journalId
  defaultLayout $ do
    $(widgetFile "journal")
