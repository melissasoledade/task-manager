{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple
import Web.Scotty (get, post, patch, scotty, html)
import Lib

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "0.0.0.0",
      connectDatabase = "docker",
      connectUser = "docker",
      connectPassword = "docker"
    }

main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Database connected!"

  routes conn
  putStrLn "Routes connected!"


routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/" $ do
    html "Olá, mundo!"

  get "/tasks" $ getTasks conn

  post "/cadastrar" $ createUser conn
