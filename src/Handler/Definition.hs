{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Definition where

import Import
import Helpers.Widgets
import Helpers.Sidebar
import Database.Persist.Sql (rawSql)
import Data.Text as T hiding (null)
import Data.Char as C (isPunctuation, isDigit)
import Network.HTTP.Types (notFound404, movedPermanently301, found302)

readFirstWithDefinition :: [Entity Mot] -> Maybe Text
readFirstWithDefinition [] = Nothing
readFirstWithDefinition (x:xs) = let m = entityVal x in
  case motDefinitionId m of
    Just _  -> if motLittre m
                  then Just $ motRacine m
                  else case xs of
                           [] -> Just $ motRacine m
                           _ -> readFirstWithDefinition xs
    Nothing -> readFirstWithDefinition xs

searchInDefinitionsForm :: Text -> Form SearchDefinitions
searchInDefinitionsForm w = renderDivs $ SearchDefinitions
                <$> aopt textField "Définition contenant" (Just $ Just w)

{-
* Ajouter à la table Mot une colonne :
    * norm : normalisé (collate strength 1 : sans accent, ni majuscule, ni ligature, etc)
    * cletri : clé de tri alphabétique (collate strength 3)
* Prévoir une normalisation à la volée du mot avec
    * Data.Text.ICU normalize
      http://hackage.haskell.org/packages/archive/text-icu/0.6.3.3/doc/html/Data-Text-ICU.html#g:6
    * Data.Text.ICU.Collate.sortKey
      http://hackage.haskell.org/packages/archive/text-icu/0.6.3.4/doc/html/Data-Text-ICU-Collate.html
-}
findFollowingWordId :: Text -> Handler (Maybe DefinitionId)
findFollowingWordId w = do
  definitionK <- runDB $ selectFirst
           [DefinitionTitreCourt >=. w, DefinitionTitreCourt !=. "sainte-mitouche"]
           [Asc DefinitionId]
  return $ fmap entityKey definitionK

getEmptyDefinitionR :: Handler Html
getEmptyDefinitionR = redirect HomeR

getDefinitionR :: Text -> Handler Html
getDefinitionR terme = do
  definitionK <- runDB $ selectFirst [DefinitionTitreUnique ==. terme] []
  case definitionK of
      Nothing -> case Import.foldr (<|>) Nothing $ candidates <*> [terme] of
          Just alt -> redirectIfAlt alt
          Nothing  -> displayNotFound terme
      Just entity -> do
                      neighbours <- findImmediateNeighbours $ entityKey entity
                      display entity neighbours
  where
      candidates :: [Text -> Maybe Text]
      candidates = [candidateParenthesis, candidateLigature, candidateFirst, candidateCaret]

      candidateParenthesis s = do
        alt <- T.findIndex ('(' ==) s
        return $ T.stripEnd $ T.take (alt - 1) s
      candidateLigature s = do
        _ <- T.findIndex ('œ' ==) s
        return $ T.replace "œ" "oe" s
      candidateFirst = T.stripSuffix ".1"
      candidateCaret = T.stripSuffix "-"

      redirectIfAlt :: Text -> Handler Html
      redirectIfAlt altTerme = do
        altdef <- runDB $ selectFirst [DefinitionTitreUnique ==. altTerme] []
        case altdef of
            Just _ -> redirectWith movedPermanently301 $ DefinitionR altTerme
            Nothing -> displayNotFound altTerme

      display e (mprev, mnext) = sidebarLayout (SidebarDefinition e) $ do
        let definition = entityVal e
        let termeNet = T.dropWhileEnd (\c -> C.isPunctuation c || C.isDigit c) terme
        let pageKeywords = mconcat [terme, ", définition, citations, étymologie, dictionnaire, Littré, français"]
        toWidgetHead [hamlet| <meta name=keywords content="#{pageKeywords}">|]
        linkrel "prev" mprev
        linkrel "next" mnext
        setTitle $ toHtml $ mconcat ["Littré - ", terme, " - définition, citations, étymologie"]
        $(widgetFile "pages/definition")
        where
            linkrel :: Html -> Maybe Definition -> Widget
            linkrel rel mdef = case mdef of
                Nothing   -> return ()
                Just ndef -> toWidgetHead [hamlet| <link rel=#{rel} href="@{DefinitionR (definitionTitreUnique ndef)}">|]

      displayNotFound mot = do
          output <- sidebarLayout SidebarRandom $ do
            setTitle "Mot non trouvé"
            toWidget [hamlet| <h1>Mot #{mot} non trouvé</h1>
                              <p>Si vous avez suivi un des 20 liens internes incorrects, merci de passer par une recherche.</p>|]
          sendResponseStatus notFound404 output

      computePage :: Definition -> Text
      computePage d = case tomePageStart $ definitionTome d of
          Nothing -> ""
          Just p  -> T.concat [tomeUrl (definitionTome d), T.pack $ show $ p + (definitionPage d)]
        where
          tomePageStart :: Int -> Maybe Int
          tomePageStart 1 = Just 68
          tomePageStart 2 = Just 7
          tomePageStart 3 = Just 8
          tomePageStart 4 = Just 7
          tomePageStart 5 = Just 13
          tomePageStart _ = Nothing

          tomeUrl :: Int -> Text
          tomeUrl 1 = "http://gallica.bnf.fr/ark:/12148/bpt6k5406710m/f"
          tomeUrl 2 = "http://gallica.bnf.fr/ark:/12148/bpt6k5406698m/f"
          tomeUrl 3 = "http://gallica.bnf.fr/ark:/12148/bpt6k5460034d/f"
          tomeUrl 4 = "http://gallica.bnf.fr/ark:/12148/bpt6k54066991/f"
          tomeUrl 5 = "http://gallica.bnf.fr/ark:/12148/bpt6k58019485/f"
          tomeUrl _ = ""

getRechercheR :: Handler Html
getRechercheR = do
  result <- runInputGet $ Recherche
                           <$> iopt textField "mot"
  case rechercheTerme result of
        Nothing -> redirectRandom
        Just terme -> do
          let searchQuery = T.strip terme
          defK <- runDB $ selectFirst [DefinitionTitreUnique ==. searchQuery] []
          case defK of
            Just entity -> redirectFromEntity entity
            Nothing -> displayAlternative searchQuery

  where
    redirectRandom :: Handler Html
    redirectRandom = getRandomWord >>= redirect . DefinitionR

    redirectFromEntity :: Entity Definition -> Handler Html
    redirectFromEntity e =
      redirectWith found302 $ DefinitionR $ definitionTitreUnique $ entityVal e

    displayAlternative :: Text -> Handler Html
    displayAlternative terme = do
        mots <- selectFormesFlechies terme
        case readFirstWithDefinition mots of
          Just racine -> redirectWith found302 $ DefinitionR racine
          Nothing -> do
            nearestId <- findFollowingWordId terme
            ((_, widgetSearchDefinitions), enctypeDef) <- runFormGet $ searchInDefinitionsForm $ escapeWordSearch terme
            content <- sidebarLayout (SidebarMaybeDefinitionId nearestId) $ do
              setTitle "Mot non trouvé"
              $(widgetFile "pages/wordNotFound")
            sendResponseStatus notFound404 content
        where escapeWordSearch :: Text -> Text
              escapeWordSearch = T.append (T.pack "=")

    selectFormesFlechies :: Text -> Handler [Entity Mot]
    selectFormesFlechies t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM mot WHERE mot = ? COLLATE french_ci ORDER BY littre DESC, (definition_id IS NULL) ASC, racine ASC, mot ASC"

    hasLittreClass :: Mot -> Bool
    hasLittreClass m = case motDefinitionId m of
      Just _  -> True
      Nothing -> False
