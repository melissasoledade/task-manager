module Lib
    ( someFunc,
      getTasks,
    ) where


import Database.PostgreSQL.Simple
import Web.Scotty (ActionM, scotty)
import qualified Web.Scotty as S

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getTasks :: Connection -> ActionM ()
getTasks conn = undefined
