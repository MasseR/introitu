module Handler.AddJournalItem where

import Import
import Handler.WriteJournal (JournalItemForm(..), journalItemForm, renderItem)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))

postAddJournalItemR :: JournalId -> Handler RepJson
postAddJournalItemR journalId = do
  ((form, _), _) <- runFormPost $ renderDivs journalItemForm
  case form of
       FormSuccess (JournalItemForm item) -> do
         created <- liftIO getCurrentTime
         itemId <- runDB $ insert $ JournalItem journalId created item
         item' <- maybe "" (wrapDiv . renderItem) <$> (runDB $ get itemId)
         jsonToRepJson $ object [("status", "ok"), ("item", htmlToText item')]
       _ -> error "Could not save"
  where
    htmlToText = TL.toStrict . renderHtml
    wrapDiv x = H.div ! A.class_ "well" $ x
