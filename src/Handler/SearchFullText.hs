module Handler.SearchFullText where

import Import
import Helpers.Sidebar
import Helpers.SphinxSearch as SS
import qualified Yesod.Paginator as P

-- 50 pages max
paginationW :: Yesod m => P.PageWidget m
paginationW = P.paginationWidget $ P.defaultPageWidgetConfig { P.maxPageLinks = Just 50 }

searchDefinitionsForm :: Form SearchDefinitions
searchDefinitionsForm = renderDivs $ SearchDefinitions
                <$> aopt textField "Définition contenant" Nothing

searchCitationsForm :: Form SearchCitations
searchCitationsForm = renderDivs $ SearchCitations
                <$> aopt textField "Citation contenant" Nothing
                <*> aopt hiddenField "" Nothing

searchProverbesForm :: Form SearchProverbes
searchProverbesForm = renderDivs $ SearchProverbes
                <$> aopt textField "Proverbe contenant" Nothing


getSearchAdvancedR :: Handler Html
getSearchAdvancedR = do
    ((_, widgetSearchDefinitions), enctypeDef) <- runFormGet searchDefinitionsForm
    ((_, widgetSearchCitations), enctypeCit) <- runFormGet searchCitationsForm
    ((_, widgetSearchProverbes), enctypeProv) <- runFormGet searchProverbesForm
    sidebarLayout SidebarRandom $ do
        setTitle "Littré - recherche avancée"
        $(widgetFile "search/index")

getSearchFullTextR :: Handler Html
getSearchFullTextR = do
  ((fresult, widgetSearchDefinitions), _) <- runFormGet searchDefinitionsForm
  currentPage <- P.getCurrentPage
  searchResults <- case fresult of
      FormMissing             -> return (0, [])
      FormSuccess formContent ->
          case searchDefinitionsQuery formContent of
              Nothing -> return (0, [])
              Just searchq -> do
                  quotedsearchq <- quoteSphinxQuery searchq
                  SS.executeSearch $ SearchDefinition quotedsearchq currentPage 
      _                       -> redirect HomeR
  let listStartAt = 1 + 20 * (currentPage - 1)
  sidebarLayout SidebarRandom $ do
      setTitle "Littré - recherche dans les définitions"
      $(widgetFile "search/definitions")

getSearchQuotationsR :: Handler Html
getSearchQuotationsR = searchQuotationsFiltered True Nothing Nothing []

getSearchQuotationsOfR :: Text -> Handler Html
getSearchQuotationsOfR authorName = do
    mauthor <- runDB $ getBy $ UniqueName authorName
    case mauthor of
        Nothing -> redirect HomeR
        Just (Entity authorId _) -> searchQuotationsFiltered False mauthor Nothing (SS.buildQuoteFilter "auteur_id" authorId)

getSearchQuotationsInR :: OeuvreId -> Text -> Handler Html
getSearchQuotationsInR opusId authorName = do
    mauthor <- runDB $ getBy $ UniqueName authorName
    mopus <- runDB $ get opusId
    case mopus of
        Nothing -> redirect HomeR
        Just _ -> searchQuotationsFiltered False mauthor mopus (SS.buildQuoteFilter "oeuvre_id" opusId) 

getSearchProverbesR :: Handler Html
getSearchProverbesR = do
  currentPage <- P.getCurrentPage
  ((fresult, widgetSearchProverbes), _) <- runFormGet searchProverbesForm
  searchResults <- case fresult of
      FormMissing             -> return (0, [])
      FormSuccess formContent ->
          case searchProverbesQuery formContent of
              Nothing      -> return (0, [])
              Just searchq -> executeSearch $ SearchProverbe searchq currentPage 
      _ -> redirect SearchAdvancedR
  let listStartAt = 1 + 20 * (currentPage - 1)
  sidebarLayout SidebarRandom $ do
      setTitle "Littré - recherche dans les proverbes"
      $(widgetFile "search/proverbes")



quoteSphinxQuery :: Text -> Handler Text
quoteSphinxQuery q = do
    ext <- lookupGetParam "ext"
    return $ case ext of
        Nothing -> SS.quote q
        Just _  -> q

searchQuotationsFiltered :: Bool -> Maybe (Entity Auteur) -> Maybe Oeuvre -> [SS.Filter] -> Handler Html
searchQuotationsFiltered required mauthor mopus sfilter = do
    currentPage <- P.getCurrentPage
    ((fresult, widgetSearchCitations), _) <- runFormGet searchCitationsForm
    searchq <- case fresult of
        FormSuccess formContent ->
            case searchCitationsQuery formContent of
                Nothing          -> return ""
                Just searchterms -> return searchterms
        _                      -> return ""
    searchResults <- if searchq == "" && required
                        then return (0, [])
                        else executeSearch $ SearchCitation searchq sfilter currentPage 
    let listStartAt = 1 + 20 * (currentPage - 1)
    sidebarLayout SidebarRandom $ do
        setTitle "Littré - recherche dans les citations"
        $(widgetFile "search/citations")
