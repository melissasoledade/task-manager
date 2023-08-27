module Main (main) where

import Database.PostgreSQL.Simple

import Lib

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "0.0.0.0",
      connectDatabase = "task-manager",
      connectUser = "teste",
      connectPassword = "postgres"
    }

main :: IO ()
main = do
    conn <- connect localPG
    someFunc
