{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Julius (RawJS (..))
import Constants

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        let (loginFormId, cpfFieldId, usernameFieldId, passwordFieldId, criarContaButtonId) = pageIds
        setTitle "Login"
        $(widgetFile "loginpage")

pageIds :: (Text, Text, Text, Text, Text)
pageIds = ("js-loginForm", "js-cpfText", "js-usernameText", "js-passwordText", "js-buttonCadastrar")
