{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      getTasks,
      createUser,
      createTask,
      updateTask,
      getLoggedUser,
    ) where


import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray.Encoding (convertToBase, Base(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Web.Scotty (ActionM, jsonData, param, post, status, text, header)
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (Status, status200, status201, status400)
import Control.Monad.IO.Class (liftIO)

-- Armazena uma tarefa
data Task = Task
    { taskId :: Int
    , name :: String
    , description :: String
    , priority :: Int
    , taskStatus :: String
    , taskUserId :: Int
    }

-- Instância para pegar dados do banco
instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

-- Converte Task para JSON
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

-- Converte JSON para Task
instance FromJSON Task where
  parseJSON (Object o) =
    Task <$> o .:? "taskId" .!= 0
      <*> o .: "name"
      <*> o .: "description"
      <*> o .: "priority"
      <*> o .: "taskStatus"
      <*> o .: "taskUserId"
  parseJSON _ = fail "Expected an object for Task"

-- Armazena usuário
data CreatedUser = CreatedUser
    { userId :: Int
    , cpf :: String
    , username :: String
    , userhash :: String
    , password :: String
    }

-- Converte usuário para JSON
instance ToJSON CreatedUser where
  toJSON (CreatedUser userId cpf username userhash password) =
    object
      [ "userId" .= userId
      , "cpf" .= cpf
      , "username" .= username
      , "userhash" .= userhash
      ]

-- Converte JSON para usuário
instance FromJSON CreatedUser where
  parseJSON (Object o) =
    CreatedUser <$> o .:? "userId" .!= 0
      <*> o .: "cpf"
      <*> o .: "username"
      <*> o .: "userhash"
      <*> o .: "password"
  parseJSON _ = fail "Expected an object for CreatedUser"

-- Instância para pegar dados do banco
instance FromRow CreatedUser where
    fromRow = CreatedUser <$> field <*> field <*> field <*> field <*> pure ""

-- Converte password em hash -> String
hashPassword :: String -> String
hashPassword password = show (hashWith SHA256 (pack password))

-- Busca usuário por CPF
getUserByCPF :: Connection -> String -> IO [CreatedUser]
getUserByCPF conn cpf = query conn "SELECT userid, cpf, username, userhash FROM users WHERE cpf = ?" (Only cpf)

-- Busca tarefa por userId
getTaskByUserId :: Connection -> Int -> IO [Task]
getTaskByUserId conn userId = query conn "SELECT taskid, name, description, priority, taskstatus, taskuserid FROM tasks WHERE taskuserid = ?" (Only userId)

-- Busca usuário na tabela users pelo userId
userExists :: Connection -> Int -> IO Bool
userExists conn _taskId = do
  [Only n] <- query conn "SELECT COUNT(*) FROM users WHERE userid = ?" (Only _taskId) :: IO [Only Int]
  return $ n > 0

-- Verifica se uma tarefa existe pelo seu id
taskExists :: Connection -> Int -> IO Bool
taskExists conn _taskId = do
  [Only n] <- query conn "SELECT COUNT(*) FROM tasks WHERE taskid = ?" (Only _taskId) :: IO [Only Int]
  return $ n > 0

-- GET de tarefas, lista todas as tarefas do banco
getTasks :: Connection -> ActionM ()
getTasks conn = do
    tasks <- (liftIO $ query_ conn "SELECT * FROM tasks") :: ActionM [Task]
    status status200
    S.json $ object ["tasks" .= tasks]

-- Cria um novo usuário. Armazena seus dados + hash de senha
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

-- Cria uma nova tarefa
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

-- Atualiza uma tarefa
updateTask :: Connection -> ActionM ()
updateTask conn = do
    _taskId <- param "id" :: ActionM Int
    _taskStatus <- param "taskStatus" :: ActionM String
    exists <- liftIO $ taskExists conn _taskId
    if not exists
        then do
            status status400
            S.json $ object ["error" .= ("Task not found" :: String)]
        else do
            let result = execute
                    conn
                    "UPDATE tasks SET taskstatus = ? WHERE taskid = ?"
                    (_taskStatus, _taskId)
            n <- liftIO result
            if n > 0
                then do
                    status status200
                    S.json $ object ["message" .= (_taskStatus :: String)]
                else do
                    status status400
                    S.json $ object ["error" .= ("Could not update task" :: String)]

-- Retorna o userId do usuário. Tentamos algumas implementações para gerar um JWT, mas não conseguimos finalizar
-- O JWT seria gerado com um set de <username, userid> + uma chave RSA
getLoggedUser :: Connection -> ActionM ()
getLoggedUser conn = do
    (CreatedUser _ _cpfLogin _usernameLogin _ _passwordLogin) <- jsonData
    userList <- liftIO $ getUserByCPF conn _cpfLogin
    if null userList
      then do
        status status400
        S.json $ object ["error" .= ("User not found" :: String)]
      else if userhash $ head userList == hashPassword _passwordLogin
        then do
          status status200
          S.json $ object ["userId" .= (show $ userId $ head userList :: String)]
        else do
          status status400
          S.json $ object ["error" .= ("Login error" :: String)]
  