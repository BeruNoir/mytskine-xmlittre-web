{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Annexes where

import Import
import Data.Text (chunksOf)
import Helpers.Sidebar


getAnnexesR :: Handler Html
getAnnexesR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Causerie"
        $(widgetFile "annexes/causerie")

getCauserieR :: Handler Html
getCauserieR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Causerie"
        $(widgetFile "annexes/causerie")

getPrononciationR :: Handler Html
getPrononciationR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Prononciation"
        $(widgetFile "annexes/prononciation")

getLivresR :: Handler Html
getLivresR = do
    let letters = chunksOf 1 "ABCDEFGHIJKLMNOPQRSTVWY"
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Livres cités"
        $(widgetFile "annexes/livres")

getPrefaceR :: Handler Html
getPrefaceR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Préface"
        $(widgetFile "annexes/preface")
