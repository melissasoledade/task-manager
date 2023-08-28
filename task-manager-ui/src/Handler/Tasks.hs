{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tasks where

import Import
import Constants
import Text.Julius (RawJS (..))

getTasksR :: Handler Html
getTasksR = defaultLayout $ do
        let (cadastroTaskFormId, nomeFieldId, priorityFieldId, descriptionFieldId, taskListId) = pageIdList
        setTitle "Tasks"
        $(widgetFile "taskspage")

pageIdList :: (Text, Text, Text, Text, Text)
pageIdList = ("js-cadastroTaskForm", "js-nomeText", "js-priorityText", "js-descriptionText", "js-taskList")
