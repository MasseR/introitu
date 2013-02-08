module Handler.WriteJournal where

import Import
import Handler.Markdown (renderMarkdown)

data JournalItemForm = JournalItemForm Textarea
journalItemForm = JournalItemForm <$> areq textareaField "" Nothing

renderItem item = let
  (Textarea content) = journalItemText item
  in renderMarkdown content

getWriteJournalR :: JournalId -> Handler RepHtml
getWriteJournalR journalId = do
  journal <- runDB $ get404 journalId
  items <- map (renderItem . entityVal) <$> (runDB $ selectList [JournalItemJournal ==. journalId] [Desc JournalItemCreated])
  ((_, formWidget), enctype) <- runFormPost $ renderDivs journalItemForm
  defaultLayout $ do
    $(widgetFile "journal")
