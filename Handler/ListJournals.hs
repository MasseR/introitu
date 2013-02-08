module Handler.ListJournals where

import Import
import Handler.CreateJournal (journalForm)

getListJournalsR :: Handler RepHtml
getListJournalsR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs (journalForm Nothing)
  (Entity user _) <- requireAuth
  journals <- runDB $ selectList [JournalOwner ==. user] []
  counts <- mapM journalItemCount journals
  let journalTuple = zip3 [(1::Int) ..] journals counts
  defaultLayout $ do
    $(widgetFile "listjournals")
  where
    journalItemCount journal = runDB $ count [JournalItemJournal ==. (entityKey journal)]
