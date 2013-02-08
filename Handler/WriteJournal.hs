module Handler.WriteJournal where

import Import

data JournalItemForm = JournalItemForm Textarea
journalItemForm = JournalItemForm <$> areq textareaField "" Nothing

getWriteJournalR :: JournalId -> Handler RepHtml
getWriteJournalR journalId = do
  journal <- runDB $ get404 journalId
  ((_, formWidget), enctype) <- runFormPost $ renderDivs journalItemForm
  defaultLayout $ do
    $(widgetFile "journal")
