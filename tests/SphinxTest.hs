{-# LANGUAGE OverloadedStrings #-}
module SphinxTest
    ( sphinxSpecs
    ) where

import TestImport

sphinxSpecs :: Spec
sphinxSpecs =
    ydescribe "/search tests (requires a running Sphinx daemon)" $ do

        yit "Search within definitions" $ do
            request $ do
                setMethod "GET"
                setUrl SearchFullTextR
                addGetParam "_hasdata" ""
                addGetParam "f1" "espace"
            statusIs 200
            htmlAllContain "h1" "Recherche en texte intégral"
            bodyContains "744 correspondances"

        yit "Search within definitions (bad word)" $ do
            request $ do
                setMethod "GET"
                setUrl SearchFullTextR
                addGetParam "_hasdata" ""
                addGetParam "f1" "trwd"
            statusIs 200
            htmlAllContain "h1" "Recherche en texte intégral"
            bodyContains "Aucun passage du Littré ne correspond"

        yit "Search within citations" $ do
            request $ do
                setMethod "GET"
                setUrl SearchQuotationsR
                addGetParam "_hasdata" ""
                addGetParam "f1" "espace"
            statusIs 200
            htmlAllContain "h1" "Recherche en texte intégral"
            bodyContains "347 correspondances"

        yit "Search within citations (exact search)" $ do
            request $ do
                setMethod "GET"
                setUrl SearchQuotationsR
                addGetParam "_hasdata" ""
                addGetParam "f1" "=espace"
            statusIs 200
            htmlAllContain "h1" "Recherche en texte intégral"
            bodyContains "294 correspondances"

        yit "Search within proverbes" $ do
            request $ do
                setMethod "GET"
                setUrl SearchProverbesR
                addGetParam "_hasdata" ""
                addGetParam "f1" "mêlé"
            statusIs 200
            htmlAllContain "h1" "Recherche dans les proverbes du Littré"
            htmlAllContain "h2" "Résultats de la recherche de proverbes"
            bodyContains "2 correspondances"
