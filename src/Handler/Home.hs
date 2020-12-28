{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Helpers.Sidebar

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Dictionnaire de la langue française"
        $(widgetFile "pages/homepage")

getFaqR :: Handler Html
getFaqR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - FAQ"
        $(widgetFile "pages/faq")

getStatsR :: Handler Html
getStatsR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Statistiques"
        $(widgetFile "pages/stats")

getProverbesR :: Handler Html
getProverbesR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Proverbes"
        $(widgetFile "pages/proverbes")

getEtymologieR :: Handler Html
getEtymologieR =
    sidebarLayout SidebarRandom $ do
        setTitle "Dictionnaire Littré - Étymologie"
        $(widgetFile "pages/etymologie")
