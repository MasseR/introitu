module Handler.View where

import Import
import Handler.Markdown (renderMarkdown)

getViewR :: NoteId -> Handler RepHtml
getViewR noteId = do
  note <- runDB $ get404 noteId
  defaultLayout $ do
    setTitle (toHtml $ noteTitle note)
    let markdown = renderMarkdown (unTextarea $ noteText note)
    $(widgetFile "view")
