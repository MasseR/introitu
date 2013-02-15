module Handler.WriteJournal where

import Import
import Handler.Markdown (renderMarkdown)
import qualified Text.Blaze.Html5 as H
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import qualified Data.Text as T (concat)

data JournalItemForm = JournalItemForm Textarea

journalItemForm :: AForm App App JournalItemForm
journalItemForm = JournalItemForm <$> areq textareaField "" Nothing

renderItem :: JournalItem -> Html
renderItem item = let
  (Textarea content) = journalItemText item
  markdown = renderMarkdown content
  date = toHtml . formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" $ journalItemCreated item
  in H.h2 date `mappend` markdown

getWriteJournalR :: JournalId -> Handler RepHtml
getWriteJournalR journalId = do
  journal <- runDB $ get404 journalId
  items <- map (renderItem . entityVal) <$> (runDB $ selectList [JournalItemJournal ==. journalId] [Desc JournalItemCreated])
  ((_, formWidget), enctype) <- runFormPost $ renderDivs journalItemForm
  defaultLayout $ do
    setTitle $ toHtml $ T.concat ["Journal - ", journalName journal]
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    $(widgetFile "journal")
