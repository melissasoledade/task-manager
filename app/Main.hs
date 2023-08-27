module Main (main) where

import Database.PostgreSQL.Simple
import Lib

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "localhost",
      connectDatabase = "docker",
      connectUser = "docker",
      connectPassword = "docker"
    }

main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Database connected!"
