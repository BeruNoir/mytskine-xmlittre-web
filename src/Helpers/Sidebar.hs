module Helpers.Sidebar
       (
           Sidebar(..),
           sidebarLayout
       ) where

import Import
import Helpers.Widgets
import Text.Hamlet (hamletFile)
import Yesod.Default.Config ()


type ActiveDefinition = Bool

data Sidebar = SidebarRandom
               | SidebarDefinition (Entity Definition)
               | SidebarMaybeDefinitionId (Maybe DefinitionId)
               | SidebarAuthor (Entity Auteur)
               | SidebarWork (Entity Oeuvre)


buildSidebar :: Sidebar -> Widget
buildSidebar SidebarRandom = buildSidebarDefinition Nothing False
buildSidebar (SidebarDefinition def) = buildSidebarDefinition (Just $ entityKey def) True
buildSidebar (SidebarMaybeDefinitionId mDefId) = buildSidebarDefinition mDefId False
buildSidebar (SidebarAuthor aut) = buildSidebarGeneric $  neighboursAuthorsWidget $ entityKey aut
buildSidebar (SidebarWork _) = buildSidebarDefinition Nothing False

buildSidebarDefinition :: Maybe DefinitionId -> ActiveDefinition -> Widget
buildSidebarDefinition defId markActive = buildSidebarGeneric $ neighboursWidget defId markActive

buildSidebarGeneric :: Widget -> Widget
buildSidebarGeneric neighbours = do
    let searchbox = $(widgetFile "widgets/searchForm")
    $(widgetFile "widgets/sidebar")


sidebarLayout :: Sidebar -> Widget -> Handler Html
sidebarLayout side widget' = do
  mmsg <- getMessage
  let sidebar = buildSidebar side
  let widget = do
      widget'
      toPageContent side
  pc <- widgetToPageContent $ do
      $(widgetFile "lucius/normalize")
      $(widgetFile "lucius/default")
      $(widgetFile "lucius/annexes")
      $(widgetFile "lucius/definition")
      $(widgetFile "lucius/search")
      $(widgetFile "lucius/other")
      $(widgetFile "sidebar-layout")
  withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  where
      toPageContent s = case s of
          SidebarDefinition def -> toWidgetHead [hamlet| <meta name="description" content="#{incipit $ entityVal def} Définition dans le Littré, dictionnaire de la langue française. Texte intégral, sans publicité ni brimborions. Définition, avec citations, historique littéraire et étymologie.">|]
          SidebarAuthor aut -> toWidgetHead [hamlet| <meta name="description" content="#{auteurNomLong $ entityVal aut} : auteur cité par le Littré, dictionnaire de la langue française. Liste des œuvres de l'auteur, recherche en texte intégral parmi les citations. Texte intégral, sans publicité ni brimborions.">|]
          SidebarWork o -> toWidgetHead [hamlet| <meta name="description" content="#{oeuvreTitre $ entityVal o} : œuvre citée par le Littré, dictionnaire de la langue française. Recherche texte en texte intégral parmi les citations. Texte intégral, sans publicité ni brimborions.">|]
          _ -> toWidgetHead [hamlet| <meta name="description" content="Le Littré, dictionnaire de la langue français. Texte intégral, sans publicité ni brimborions. Dictionnaire ancien, paru de 1873 à 1877 en 4 volumes et un supplément. Les définitions sont agrémentées de nombreuses citations littéraires allant du Xe au XIXe siècle.">|]

      incipit :: Definition -> Text
      incipit d = mconcat [definitionTitre d, " : ", definitionIncipit d]

