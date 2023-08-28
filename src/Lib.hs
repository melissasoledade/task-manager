{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      getTasks,
    ) where


import Data.Aeson
import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Web.Scotty (ActionM, jsonData, param, post, status, text)
import qualified Web.Scotty as S
import Data.Aeson (ToJSON, object, (.=))
import Network.HTTP.Types.Status (Status, status200, status201, status204, status400)
import Control.Monad.IO.Class (liftIO)


data Task = Task
    { taskId :: Int
    , name :: String
    , description :: String
    , priority :: Int
    , taskStatus :: String
    , userId :: Int
    }

instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Task where
  toJSON (Task taskId name description priority taskStatus userId) =
    object
      [ "taskId" .= taskId
      , "name" .= name
      , "description" .= description
      , "priority" .= priority
      , "taskStatus" .= taskStatus
      , "userId" .= userId
      ]

getTasks :: Connection -> ActionM ()
getTasks conn = do
    tasks <- (liftIO $ query_ conn "SELECT * FROM tasks") :: ActionM [Task]
    status status200
    S.json $ object ["tasks" .= tasks]
