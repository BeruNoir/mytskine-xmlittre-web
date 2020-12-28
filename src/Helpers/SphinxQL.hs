module Helpers.SphinxQL
       (
           Filter(..)
         , toSphinxQL
         , quote
         , buildQuoteFilter
         ) where

import qualified Data.Text as T
import Data.Int (Int64)
import Database.Persist.Sql (SqlBackend, fromSqlKey)
import Database.Persist.Class (ToBackendKey, Key)
import Prelude

-- | Filter types
data Filter = ExclusionFilter Filter
            | FilterValues String [Int64]
            | FilterRange  String Int64 Int64
            deriving (Show)

toSphinxQL :: Filter -> T.Text
toSphinxQL (ExclusionFilter f) = T.concat ["NOT (", toSphinxQL f, ")"]
toSphinxQL (FilterValues field vs) = T.concat [T.pack field, " IN (", (T.intercalate "," (map (T.pack . show) vs)), ")"]
toSphinxQL (FilterRange field a b) = T.concat [T.pack field, " BETWEEN ", T.pack $ show a, " AND ", T.pack $ show b]

quote :: T.Text -> T.Text
quote = T.foldl filterchar ""
  where -- filterchar :: T.Text -> T.Char -> T.Text
        filterchar s '|' = s
        filterchar s '!' = s
        filterchar s '-' = s
        filterchar s '@' = s
        filterchar s '(' = s
        filterchar s ')' = s
        filterchar str c = T.snoc str c

buildQuoteFilter :: ToBackendKey SqlBackend record => String -> Key record -> [Filter]
buildQuoteFilter filterCol filterVal = [FilterValues filterCol [fromSqlKey filterVal]]
