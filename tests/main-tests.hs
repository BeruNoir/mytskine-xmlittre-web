{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)
import Settings (parseExtra)

import BaseTest
import DefinitionTest
import LinksTest
import SphinxTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    wordsInHome <- getLinks "homepage.hamlet"
    hspec $ do
        yesodSpec foundation $ do
            baseSpecs
            routeSpecs
            definitionSpecs
            definitionRedirSpecs
            searchDefinitionSpecs
            linksSpecs wordsInHome
            sphinxSpecs
