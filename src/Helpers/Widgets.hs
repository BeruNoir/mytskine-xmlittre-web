module Helpers.Widgets
       (
           neighboursWidget
         , neighboursAuthorsWidget
         , getRandomWord
         , WordLink (..)
         , WordLinkType (..)
         , AuthorLink (..)
         , findImmediateNeighbours
       ) where

import Import

import System.Random (getStdRandom, randomR)
import Database.Persist.Sql
import Data.Int (Int64)
import Data.List as L (head, last)

data WordLink = WordLink {
    wordLinkDefinitionId :: DefinitionId
  , wordLinkShortTitle :: Text
  , wordLinkUniqueTitle :: Text
  , wordLinkType :: WordLinkType
  }
data WordLinkType = ActiveWord | PreviousWord | NextWord | NormalWord


-- neighboursNum :: GHC.Int.Int64
neighboursNum :: Int64
neighboursNum = 10

maxDefId :: Integer
maxDefId = 78600

randDefId :: IO Integer
randDefId = getStdRandom (randomR (minV, maxV))
  where
    minV = (fromIntegral neighboursNum) + 1
    maxV = maxDefId - (fromIntegral neighboursNum) - 1

findImmediateNeighbours :: DefinitionId -> Handler (Maybe Definition, Maybe Definition)
findImmediateNeighbours did = do
    let center = fromSqlKey did
    neighbours <- runDB $ selectList ([DefinitionId ==. toSqlKey (center - 1)] ||. [DefinitionId ==. toSqlKey (center + 1)]) [Asc DefinitionId]
    let before = if center == 1 then Nothing else Just $ L.head neighbours
    let after  = if center == fromIntegral maxDefId then Nothing else Just $ L.last neighbours
    return $ mapTup (fmap entityVal) (before, after)
  where
    mapTup f (a1, a2) = (f a1, f a2)

computeDefId :: Maybe DefinitionId -> IO Int64
computeDefId d = case d of
  Just did -> return $ fromSqlKey did
  Nothing  -> do
    did <- randDefId
    return $ fromIntegral did

valueToInterval :: Int64 -> [PersistValue]
valueToInterval i = 
  map toPersistValue [
        PersistInt64 $ fromIntegral (i - neighboursNum), 
        PersistInt64 $ fromIntegral (i + neighboursNum)
        ]

findNeighbours :: Maybe DefinitionId -> Bool -> Handler [WordLink]
findNeighbours defId markActive = do
  did <- liftIO $ computeDefId defId
  results <- getEntryList (valueToInterval did)
  return $ map (
    \(Single d, Single tc, Single tu) -> WordLink d (toText tc) (toText tu) (setWordType d)
    ) results
  where
      setWordType :: DefinitionId -> WordLinkType
      setWordType myid = 
          if markActive 
          then case defId of
              Just myid' -> equalOrNext myid myid'
              _          -> NormalWord
          else NormalWord

      equalOrNext :: DefinitionId -> DefinitionId -> WordLinkType
      equalOrNext x y
          | x == y = ActiveWord
          | x == incKey y = NextWord
          | otherwise = NormalWord

      getEntryList :: [PersistValue] -> Handler [(Single DefinitionId, Single PersistValue, Single PersistValue)]
      getEntryList vals = runDB $
          rawSql "SELECT id, titre_court, titre_unique FROM definition WHERE id BETWEEN ? AND  ? ORDER BY id ASC" vals

      incKey :: DefinitionId -> DefinitionId
      incKey k = toSqlKey $ 1 + fromSqlKey k

neighboursWidget :: Maybe DefinitionId -> Bool -> Widget
neighboursWidget defId markActive = do
  definitions <- handlerToWidget $ findNeighbours defId markActive
  $(widgetFile "widgets/neighbours")

getRandomWord :: Handler Text
getRandomWord = do
  did <- liftIO $ computeDefId Nothing
  result <- runDB $ get $ toSqlKey did
  case result of
    Just entry -> return $ definitionTitreUnique entry
    Nothing -> return "erreur"



data AuthorLink = AuthorLink {
    alNomCourt :: Text
  , alNomLong  :: Text
  , alLinkType :: WordLinkType
  }

neighboursAuthorsWidget :: AuteurId -> Widget
neighboursAuthorsWidget aId = do
    authors <- handlerToWidget $ findNeighbouringAuthors aId
    $(widgetFile "widgets/neighbouringAuthors")
    where
        findNeighbouringAuthors :: AuteurId -> Handler [AuthorLink]
        findNeighbouringAuthors aid = do
            results <- getEntryList (valueToInterval $ unSqlBackendKey $ unAuteurKey aid)
            return $ map rowToRecord results

        getEntryList :: [PersistValue] -> Handler [(Single PersistValue, Single PersistValue)]
        getEntryList vals = runDB $
            rawSql "SELECT nom_court, nom_long FROM auteur WHERE id BETWEEN ? AND  ? ORDER BY id ASC" vals

        rowToRecord :: (Single PersistValue, Single PersistValue) -> AuthorLink
        rowToRecord (Single a, Single b) = AuthorLink (toText a) (toText b) NormalWord



toText :: PersistValue -> Text
toText = nullToEmpty . fromPersistValue
    where
        nullToEmpty (Left _)  = ""
        nullToEmpty (Right x) = x

