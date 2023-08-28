{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cadastrar where

import Import
import Text.Julius (RawJS (..))
import Constants

getCadastrarR :: Handler Html
getCadastrarR = defaultLayout $ do
        let (cadastroFormId, cpfFieldId, usernameFieldId, passwordFieldId) = pageIdsCadastrar
        setTitle "Cadastrar"
        $(widgetFile "cadastropage")

pageIdsCadastrar :: (Text, Text, Text, Text)
pageIdsCadastrar = ("js-loginForm", "js-cpfText", "js-usernameText", "js-passwordText")
