{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Oeuvre where

import Import
import Helpers.Sidebar
import Data.Text as Text (replace)
import Network.HTTP.Types (notFound404)

getOeuvreR :: Text -> Handler Html
getOeuvreR opusName = do
  maybeoeuvre <- runDB $ selectFirst [OeuvreTitreUnique ==. Just opusName] []
  case maybeoeuvre of
      Nothing -> do
        output <- sidebarLayout SidebarRandom $ [whamlet|<h1>Œuvre non trouvée|]
        sendResponseStatus notFound404 output
      Just (Entity oid oeuvre) -> do
        countCitations <- runDB $ count [CitationOeuvreId ==. Just oid]
        sidebarLayout (SidebarWork $ Entity oid oeuvre) $ do
          setTitle $ toHtml $ mconcat [oeuvreTitre oeuvre, " - œuvre citée dans le Littré"]
          $(widgetFile "pages/oeuvre")
  where
      prettifyUrlComponent :: Oeuvre -> Text
      prettifyUrlComponent = (Text.replace " " "_") . oeuvreTitre
