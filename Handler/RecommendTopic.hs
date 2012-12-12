module Handler.RecommendTopic where

import Import

getRecommendTopicR :: Handler RepJson
getRecommendTopicR = do
  topics <- runDB $ map (noteTopic . entityVal) <$> selectList [] []
  jsonToRepJson $ topics
