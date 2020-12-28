{-# LANGUAGE OverloadedStrings #-}
module DefinitionTest
    ( definitionSpecs
    , definitionRedirSpecs
    , searchDefinitionSpecs
    ) where

import TestImport

definitionSpecs :: Spec
definitionSpecs =
    ydescribe "/definition tests" $ do

        yit "Check the definition of an unknown word" $ do
            get $ DefinitionR "jorgeluisborges"
            statusIs 404
            htmlAllContain "h1" "jorgeluisborges"

        yit "Check the definition of paria" $ do
            get $ DefinitionR "paria"
            statusIs 200
            htmlCount "h1" 1
            htmlAllContain "h1" "paria"
            htmlAllContain "ul.corps" "Homme de la dernière caste"
            htmlAnyContain "q" "le chef des raispeutes"
            htmlAnyContain "a.aut" "Bernardin de Saint-Pierre"

        yit "Check the definition of pointer.2" $ do
            get $ DefinitionR "pointer.2"
            statusIs 200
            htmlAllContain "h1" "pointer.2"
            htmlAnyContain "li.v" "chien anglais"

        yit "Check the definition of fêté" $ do
            get $ DefinitionR "fêté"
            statusIs 200
            htmlCount "h1" 1
            -- printBody
            -- printMatches "h1"
            htmlAllContain "h1" "fêté"
            htmlAnyContain "q" "porte de la Gaîté"

        yit "Check the definition of boeuf" $ do
            get $ DefinitionR "boeuf"
            statusIs 200
            htmlAllContain "h1" "boeuf"
            htmlAllContain "h2" "bœuf"

        yit "Check the definition of 'moquer'" $ do
            get $ DefinitionR "moquer"
            statusIs 200
            htmlAllContain "h2" "moquer (se)"

        yit "Check the definition of 'garde.4'" $ do
            get $ DefinitionR "garde.4"
            statusIs 200

definitionRedirSpecs :: Spec
definitionRedirSpecs =
    ydescribe "/definition tests with redirection" $ do

        yit "Check the definition of bœuf" $ do
            get $ DefinitionR "bœuf"
            statusIs 301

        yit "Check the definition of 'moquer (se)'" $ do
            get $ DefinitionR "moquer (se)"
            statusIs 301

        yit "Check the definition of 'goutte.1'" $ do
            get $ DefinitionR "goutte.1"
            statusIs 301

        yit "Check the definition of 'mono-'" $ do
            get $ DefinitionR "mono-"
            statusIs 301

searchDefinitionSpecs :: Spec
searchDefinitionSpecs =
    ydescribe "definition search tests" $ do
        yit "Search an empty definition" $ do
            search "" 303
        yit "Search the definition of fêté" $ do
            search "fêté" 302
            assertHeader "Location" "http://littre:3000/definition/f%C3%AAt%C3%A9"
        yit "Search the definition of un unknown word" $ do
            search "tralalalalala" 404
            htmlAllContain "h1" "inconnu du Littré"
        yit "Check the definition of 'manges'" $ do
            search "manges" 302
        yit "Check the definition of 'mangées'" $ do
            search "mangées" 302
  where
    search word code = do
        request $ do
            setMethod "GET"
            setUrl RechercheR
            addGetParam "mot" word
        statusIs code
