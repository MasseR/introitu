module Handler.CreateJournal where

import Import

data JournalForm = JournalForm {
  fJournalName :: Text,
  fJournalDescription :: (Maybe Textarea)
  }
journalForm j = JournalForm
  <$> areq textField "Name" (fJournalName <$> j)
  <*> aopt textareaField "Description" (fJournalDescription <$> j)

getCreateJournalR :: Handler RepHtml
getCreateJournalR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs (journalForm Nothing)
  defaultLayout $ do
    setTitle "New journal"
    $(widgetFile "createjournal")

postCreateJournalR :: Handler RepHtml
postCreateJournalR = error "Not yet implemented: postCreateJournalR"
