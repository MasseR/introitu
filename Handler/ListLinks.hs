module Handler.ListLinks where

import Import
import Control.Monad (forM)
import qualified Data.Text as T

findLinks :: UserId -> YesodDB sub App [(Link, [Text])]
findLinks user = do
  links <- selectList [LinkOwner ==. user] []
  forM links $ \(Entity linkid link) -> do
    tags <- map (linkTagsTag . entityVal) <$> selectList [LinkTagsLink ==. linkid] []
    return (link, tags)


getListLinksR :: Handler RepHtml
getListLinksR = do
  (Entity user _) <- requireAuth
  links <- runDB $ findLinks user
  defaultLayout $ do
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.3.3/underscore-min.js"
    addScriptRemote "https://raw.github.com/raimohanska/bacon.js/master/lib/Bacon.min.js"
    $(widgetFile "listLinks")
