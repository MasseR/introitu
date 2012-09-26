module Handler.Edit where

import Import hiding (update, (==.), (=.))
import Handler.Add (NoteForm(..), noteForm)
import Handler.Markdown (renderMarkdown, htmlToText)
import Database.Esqueleto
import Model.Note

modifyNote :: NoteId -> NoteForm -> YesodDB sub App ()
modifyNote noteId form = update $ \n -> do
  set n [NoteTitle =. val (fnoteTitle form),
         NoteText =. val (fnoteContent form),
         NoteTopic =. val (fnoteTopic form)]
  where_ $ (n ^. NoteId ==. val noteId)

getEditR :: NoteId -> Handler RepHtml
getEditR noteId = do
  note <- runDB $ get404 noteId
  let form = Just $ NoteForm (noteTitle note) (noteText note) (noteTopic note)
  ((_, formWidget), enctype) <- runFormPost $ renderDivs $ noteForm form
  defaultLayout $ do
    let title = noteTitle note
    let header = $(widgetFile "editorheader")
        markdown = renderMarkdown (unTextarea $ noteText note)
    setTitle $ toHtml title
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.3.3/underscore-min.js"
    $(widgetFile "preview")
    $(widgetFile "editor")

postEditR :: NoteId -> Handler RepHtml
postEditR = error "Not yet implemented: postEditR"

putEditR :: NoteId -> Handler RepJson
putEditR noteId = do
  _ <- runDB $ get404 noteId
  ((result, _), _) <- runFormPost $ renderDivs (noteForm Nothing)
  case result of
       FormSuccess form -> do
         time <- liftIO getCurrentTime
         _ <- runDB $ modifyNote noteId form
         _ <- getUrlRender >>= indexNote noteId time
         jsonToRepJson $ object [("status", "ok" :: Text), ("rendered", htmlToText $ renderMarkdown (unTextarea $ fnoteContent form))]
       err -> jsonToRepJson $ object [("error", show err)]
