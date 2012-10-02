module Handler.List where

import Import
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Control.Arrow ((***))
import Data.List (last)

paginateSelect :: (PersistEntity val, PersistQuery (PersistEntityBackend val) m) =>
     Int -- How many items in a page
   -> Int -- Which page we're looking at
   -> [Filter val] -- Query condition
   -> [SelectOpt val] -- Sortings etc
   -> PersistEntityBackend val m (Int, [Entity val])
paginateSelect itemsPerPage pageNum qfilter qsel = do
  let offset = itemsPerPage * (pageNum - 1)
  pages <- calculatePages <$> count qfilter
  items <- selectList qfilter $ qsel ++ [LimitTo itemsPerPage, OffsetBy offset]
  return (pages, items)
  where
    calculatePages :: Int -> Int
    calculatePages items = ceiling (fromIntegral items / fromIntegral itemsPerPage :: Double)

getListR :: Handler RepHtml
getListR = do
  pageNum <- maybe 1 id <$> (runInputGet $ iopt intField "page")
  user <- requireAuth
  let itemsPerPage = 15
      qfilter = [NoteOwner ==. entityKey user]
      qsel = [Asc NoteTopic]
  (pages, notes) <- ((enumFromTo 1) *** (zip [1 :: Int ..])) <$>
    (runDB $ paginateSelect itemsPerPage pageNum qfilter qsel)
  defaultLayout $ do
    let title = "Your notes" :: Html
    setTitle title
    $(widgetFile "list")
  where
    spoiler (Textarea text) = maybe "" id $ listToMaybe $ T.lines text
