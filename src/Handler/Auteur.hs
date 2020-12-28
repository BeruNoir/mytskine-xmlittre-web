{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Auteur where

import Import
import Helpers.Sidebar
import Helpers.SphinxSearch (listRandomQuotes, SphinxResult(..))
import Data.Text as Text (replace, stripSuffix, toUpper)
import Network.HTTP.Types (notFound404, movedPermanently301)

getAuteurR :: Text -> Handler Html
getAuteurR nom = do
  let searchName = Text.replace "_" " " nom
  aut <- runDB $ getBy $ UniqueName searchName
  case aut of
      Nothing -> case Import.foldr (<|>) Nothing $ [candidatePeriod, candidateCase] <*> [nom] of
          Just alt -> redirectIfAlt alt
          Nothing -> displayNotFound searchName
      Just entity -> do
        let auteur = entityVal entity
        let auteurId = entityKey entity
        oeuvres <- runDB $ selectList [OeuvreAuteurId ==. Just auteurId] [Asc OeuvreId]
        citations <- listRandomQuotes auteurId
        sidebarLayout (SidebarAuthor entity) $ do
          setTitle $ toHtml $ mconcat [nom, ", auteur cité dans le Littré"]
          $(widgetFile "pages/auteur")
  where
      candidatePeriod = Text.stripSuffix "."

      candidateCase w = let up = Text.toUpper w in
          if up == w then Nothing
                    else Just up

      redirectIfAlt altNom = do
        altdef <- runDB $ getBy $ UniqueName altNom
        case altdef of
            Just _ -> redirectWith movedPermanently301 $ AuteurR altNom
            Nothing -> displayNotFound altNom

      displayNotFound nominc = do
          output <- sidebarLayout SidebarRandom $ do
            setTitle "Auteur non trouvé"
            toWidget [hamlet| <h1>Auteur #{nominc} non trouvé</h1>
                              <p>Si vous avez suivi un lien interne au site littre.org, alors l'erreur sera vérifiée et corrigée prochainement.</p>|]
          sendResponseStatus notFound404 output
