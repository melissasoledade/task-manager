{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      getTasks,
      createUser,
      hashPassword,
      createTask,
    ) where


import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray.Encoding (convertToBase, Base(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
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
    , password :: String
    }

instance ToJSON CreatedUser where
  toJSON (CreatedUser userId cpf username userhash password) =
    object
      [ "userId" .= userId
      , "cpf" .= cpf
      , "username" .= username
      , "userhash" .= userhash
      ]

instance FromJSON CreatedUser where
  parseJSON (Object o) =
    CreatedUser <$> o .:? "userId" .!= 0
      <*> o .: "cpf"
      <*> o .: "username"
      <*> o .: "userhash"
      <*> o .: "password"
  parseJSON _ = fail "Expected an object for CreatedUser"

instance FromRow CreatedUser where
    fromRow = CreatedUser <$> field <*> field <*> field <*> field <*> pure ""

hashPassword :: String -> String
hashPassword password = show (hashWith SHA256 (pack password))

getTasks :: Connection -> ActionM ()
getTasks conn = do
    tasks <- (liftIO $ query_ conn "SELECT * FROM tasks") :: ActionM [Task]
    status status200
    S.json $ object ["tasks" .= tasks]

getUserByCPF :: Connection -> String -> IO [CreatedUser]
getUserByCPF conn cpf = query conn "SELECT userid, cpf, username, userhash FROM users WHERE cpf = ?" (Only cpf)

getTaskByUserId :: Connection -> Int -> IO [Task]
getTaskByUserId conn userId = query conn "SELECT taskid, name, description, priority, taskstatus, taskuserid FROM tasks WHERE taskuserid = ?" (Only userId)

createUser :: Connection -> ActionM ()
createUser conn = do
    (CreatedUser _ _cpf _username _ _password) <- jsonData
    let hashedPassword = hashPassword _password
    let result = execute
                    conn
                    "INSERT INTO users (cpf, username, userhash) VALUES (?, ?, ?)"
                    (_cpf, _username, hashedPassword)
    n <- liftIO result
    if n > 0
        then do
            user <- liftIO $ getUserByCPF conn _cpf
            status status201
            S.json $ object ["user" .= user]
        else do
            status status400


createTask :: Connection -> ActionM ()
createTask conn = do
    (Task _ _name _description _priority _taskStatus _taskUserId) <- jsonData
    let result = execute
                    conn
                    "INSERT INTO tasks (name, description, priority, taskstatus, taskuserid) VALUES (?, ?, ?, ?, ?)"
                    (_name, _description, _priority, _taskStatus, _taskUserId)
    n <- liftIO result
    if n > 0
        then do
            tasks <- liftIO $ getTaskByUserId conn _taskUserId
            status status201
            S.json $ object ["tasks" .= tasks]
        else do
            status status400


-- generateJwt :: CreatedUser -> String
-- generateJwt (CreatedUser _userId _cpf _username _userhash _) = do

-- getLoggedUser :: Connection -> ActionM ()
-- getLoggedUser conn = do
--     (CreatedUser _ _cpf _username _ _password) <- jsonData
