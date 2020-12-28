{-# LANGUAGE OverloadedStrings #-}
module BaseTest
    ( baseSpecs
    , routeSpecs
    ) where

import TestImport

baseSpecs :: Spec
baseSpecs =
    ydescribe "Base tests" $ do

        yit "Check the dictionary size" $ do
            defCount <- runDB $ count ([] :: [Filter Definition])
            assertEqual "More than 78420 definitions" True $ defCount > 78420
            writeLn $ "    Definitions: " ++ (show defCount)

        yit "Check the Gallica links" $ do
            pagesCount <- runDB $ count [DefinitionPage >. 0]
            assertEqual "More than 78000 page numbers" True $ pagesCount > 78000

routeSpecs :: Spec
routeSpecs =
    ydescribe "Simple route tests" $ do

        yit "Check the home page" $ do
            get HomeR
            statusIs 200
            htmlAllContain "h1" "Le Littré"
            htmlCount "h2" 3

        yit "Check author LA BRUYÈRE" $ do
            get $ AuteurR "LA_BRUYÈRE"
            statusIs 200
            htmlAllContain "h2" "LA BRUYÈRE"
            htmlAllContain "h1" "Jean de LA BRUYÈRE"
            htmlCount "h3" 2

        yit "Check author Th. CORNEILLE" $ do
            get $ AuteurR "Th. CORNEILLE"
            statusIs 200
            htmlAllContain "h2" "Th. CORNEILLE"
            htmlAllContain "h1" "Thomas CORNEILLE"
            htmlCount "h3" 2

        yit "Check author LALALA" $ do
            get $ AuteurR "LALALA"
            statusIs 404

        yit "Check opus Vie du pape Grégoire le Grand" $ do
            get $ OeuvreR "Vie_du_pape_Grégoire_le_Grand"
            statusIs 200
            htmlAllContain "h1" "Vie du pape Grégoire le Grand"
            htmlCount "h2" 1
            htmlCount "h3" 0

        yit "Check opus LALALA" $ do
            get $ OeuvreR "LALALA"
            statusIs 404

