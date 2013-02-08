module Handler.CreateJournal where

import Import

data JournalForm = JournalForm {
  fJournalName :: Text,
  fJournalDescription :: (Maybe Textarea)
  }
journalForm :: Maybe JournalForm -> AForm App App JournalForm
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
postCreateJournalR = do
  (Entity userId _) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (journalForm Nothing)
  case result of
       FormSuccess journal -> do
         journalId <- runDB $ insert $ Journal userId (fJournalName journal) (fJournalDescription journal)
         redirect $ WriteJournalR journalId
       _ -> redirect CreateJournalR
