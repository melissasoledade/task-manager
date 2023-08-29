{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple
import Web.Scotty (ActionM, jsonData, param, post, status, text, header, options, header, patch, get, middleware, scotty, addHeader)
import Network.HTTP.Types.Status (Status, status200, status201, status400)
import Network.Wai.Middleware.Cors (simpleCors)
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
  -- matchAny  "/" $ text "Success"

  conn <- connect localPG
  putStrLn "Database connected!"

  routes conn
  putStrLn "Routes connected!"

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/tasks" $ getTasks conn
  options "/tasks" $ do
    addHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept, X-userID"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS, PATCH"
    addHeader "Access-Control-Allow-Credentials" "true"
    status status200

  post "/cadastrar" $ createUser conn
  options "/cadastrar" $ do
    addHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept, X-userID"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS, PATCH"
    addHeader "Access-Control-Allow-Credentials" "true"
    status status200

  post "/tasks" $ createTask conn

  post "/login" $ getLoggedUser conn
  options "/login" $ do
    addHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept, X-userID"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS, PATCH"
    addHeader "Access-Control-Allow-Credentials" "true"
    status status200

  patch "/tasks/:id" $ updateTask conn
  options "/tasks/:id" $ do
    addHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept, X-userID"
    addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS, PATCH"
    addHeader "Access-Control-Allow-Credentials" "true"
    status status200
