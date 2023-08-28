{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      getTasks,
      createUser,
    ) where


import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash (hash, SHA256)
import Data.ByteArray.Encoding (convertToBase, Base(..))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Web.Scotty (ActionM, jsonData, param, post, status, text)
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (Status, status200, status201, status204, status400)
import Control.Monad.IO.Class (liftIO)


data Task = Task
    { taskId :: Int
    , name :: String
    , description :: String
    , priority :: Int
    , taskStatus :: String
    , taskUserId :: Int
    }

instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Task where
  toJSON (Task taskId name description priority taskStatus taskUserId) =
    object
      [ "taskId" .= taskId
      , "name" .= name
      , "description" .= description
      , "priority" .= priority
      , "taskStatus" .= taskStatus
      , "taskUserId" .= taskUserId
      ]

instance FromJSON Task where
  parseJSON (Object o) =
    Task <$> o .:? "taskId" .!= 0
      <*> o .: "name"
      <*> o .: "description"
      <*> o .: "priority"
      <*> o .: "taskStatus"
      <*> o .: "taskUserId"
  parseJSON _ = fail "Expected an object for Task"

data CreatedUser = CreatedUser
    { userId :: Int
    , cpf :: String
    , username :: String
    , userhash :: String
    }

instance ToJSON CreatedUser where
  toJSON (CreatedUser userId cpf username userhash) =
    object
      [ "userId" .= userId
      , "cpf" .= cpf
      , "username" .= username
      , "userhash" .= userhash
      ]

instance FromRow CreatedUser where
    fromRow = CreatedUser <$> field <*> field <*> field <*> field

data User = User
    { reqCpf :: String
    , reqUsername :: String
    , password:: String
    }

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance FromJSON User where
    parseJSON (Object o) =
        User <$> o .: "reqCpf"
             <*> o .: "reqUsername"
             <*> o .: "password"
    parseJSON _ = fail "Expected an object for User"



getTasks :: Connection -> ActionM ()
getTasks conn = do
    tasks <- (liftIO $ query_ conn "SELECT * FROM tasks") :: ActionM [Task]
    status status200
    S.json $ object ["tasks" .= tasks]


createUser :: Connection -> ActionM ()
createUser conn = do
    (User _reqCpf _reqUsername _password) <- jsonData
    let result = execute
                    conn
                    "INSERT INTO users (cpf, username, userhash) VALUES (?, ?, ?)"
                    (_reqCpf, _reqUsername, _password)
    n <- liftIO result
    if n > 0
        then do
            status status201
        else do
            status status400