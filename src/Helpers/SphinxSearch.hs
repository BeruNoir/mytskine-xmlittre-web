{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helpers.SphinxSearch
       (
         SphinxResult (..)
       , SearchDefinition (..)
       , SearchCitation (..)
       , SearchProverbe (..)
       , executeSearch
       , listRandomQuotes
       , S.buildQuoteFilter
       , S.quote
       , S.Filter (..)
       ) where

import Import

import Database.Persist.MySQL (MySQLConf(..))
import Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Types as MySQL
import Database.MySQL.Simple.QueryResults as MySQL
import Database.MySQL.Simple.Result as MySQL

import qualified Data.Text as T
import Data.Text.Encoding as E (encodeUtf8)
import Helpers.SphinxQL as S

import Data.ByteString (ByteString)
import Text.Blaze.Html (preEscapedToHtml)

type TotalFound = Int
type PageNumber = Int

instance MySQL.Result Html where
    convert f = preEscapedToHtml . toText
        where toText :: Maybe ByteString -> T.Text
              toText = convert f


data SearchDefinition = SearchDefinition Text PageNumber

data SearchCitation = SearchCitation Text [S.Filter] PageNumber

data SearchProverbe = SearchProverbe Text PageNumber

data SphinxResult a where
    SphinxResultDefinition ::
      { title :: T.Text
      , utitle :: T.Text
      , excerpt :: Html
      , relevance :: Int
      } -> SphinxResult SearchDefinition
    SphinxResultCitation ::
      { titlec :: T.Text
      , utitlec :: T.Text
      , citation :: Html
      , auteurcourt :: Text
      , auteur :: Text
      , oeuvre :: Text
      , reference :: Text
      , relevancec :: Int
      } -> SphinxResult SearchCitation
    SphinxResultProverbe ::
      { proverbe :: T.Text
      , explication :: Html
      , relevancep :: Int
      } -> SphinxResult SearchProverbe

instance MySQL.QueryResults (SphinxResult SearchDefinition) where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = SphinxResultDefinition a b c d
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
              !d = convert fd vd
    convertResults fs vs  = convertError fs vs 4

instance MySQL.QueryResults (SphinxResult SearchCitation) where
    convertResults [fcit,fac,fa,fo,fref,ft,fut,frel] [vcit,vac,va,vo,vref,vt,vut,vrel] =
          SphinxResultCitation t ut cit ac a o ref rel
        where !t = convert ft vt
              !ut = convert fut vut
              !cit = convert fcit vcit
              !ac = convert fac vac
              !a = convert fa va
              !o = convert fo vo
              !ref = convert fref vref
              !rel = convert frel vrel
    convertResults fs vs  = convertError fs vs 8

instance MySQL.QueryResults (SphinxResult SearchProverbe) where
    convertResults [fa,fb,fc] [va,vb,vc] = SphinxResultProverbe a b c
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
    convertResults fs vs = convertError fs vs 3



class SSearch a where
    executeSSearch :: MySQL.Connection -> a -> Handler [SphinxResult a]
    logmsg :: a -> T.Text

instance SSearch SearchDefinition where
  executeSSearch mysqlconn (SearchDefinition qstring page) = do
    $(logDebugS) "Sphinx" sql
    res <- liftIO $ MySQL.query mysqlconn (MySQL.Query $ E.encodeUtf8 sql) (qstring, qstring)
    return res
    where
      limit = 20 :: Int
      offset = limit * (page - 1)
      sql = T.concat [
        "SELECT titre title, titre_unique utitle, SNIPPET(texte, ?) excerpt, WEIGHT() relevance "
        , "FROM definition"
        , " WHERE MATCH(?)"
        , " ORDER BY WEIGHT() DESC"
        , " LIMIT ", (T.pack $ show offset), ",", (T.pack $ show limit)
        , " OPTION field_weights=(titre=10,texte=1)" -- only for definition
        ]
  logmsg (SearchDefinition qstring page) = T.concat [
            "Définitions | QUERY:", (T.pack $ show qstring ++ " PAGE:" ++ show page)]

instance SSearch SearchCitation where
  executeSSearch mysqlconn (SearchCitation qstring filters page) = do
    $(logDebugS) "Sphinx" sql
    res <- liftIO $ MySQL.query mysqlconn (MySQL.Query $ E.encodeUtf8 sql) (qstring, qstring)
    return res
    where
      limit = 20 :: Int
      offset = limit * (page - 1)
      sql = T.concat [
        "SELECT SNIPPET(texte, ?, 'limit=0') AS cit, auteurcourt, auteur, oeuvre, reference, titre AS title, titre_unique utitle, WEIGHT() relevance "
        , "FROM citation"
        , " WHERE ", (T.intercalate " AND " $ " MATCH(?) " : (map toSphinxQL filters))
        , " ORDER BY WEIGHT() DESC"
        , " LIMIT ", (T.pack $ show offset), ",", (T.pack $ show limit)
        ]
  logmsg (SearchCitation qstring filters page) = T.concat [
            "Citations | QUERY:", (T.pack $ show qstring ++ " FILTERS:" ++ show filters ++ " PAGE:" ++ show page)]

instance SSearch SearchProverbe where
  executeSSearch mysqlconn (SearchProverbe qstring page) = do
    $(logDebugS) "Sphinx" sql
    res <- liftIO $ MySQL.query mysqlconn (MySQL.Query $ E.encodeUtf8 sql) [qstring]
    return res
    where
      limit = 20 :: Int
      offset = limit * (page - 1)
      sql = T.concat [
        "SELECT proverbe, explication, WEIGHT() AS relevance "
        , "FROM proverbe"
        , " WHERE MATCH(?) "
        , " ORDER BY WEIGHT() DESC"
        , " LIMIT ", (T.pack $ show offset), ",", (T.pack $ show limit)
        ]
  logmsg (SearchProverbe qstring page) = T.concat [
            "Proverbes | QUERY:", (T.pack $ show qstring ++ " PAGE:" ++ show page)]


executeSearch :: (SSearch a) => a -> Handler (TotalFound, [SphinxResult a])
executeSearch s = do
    $(logInfoS) "Sphinx" $ logmsg s
    y <- getYesod
    mysqlconn <- liftIO $ MySQL.connect (myConnInfo $ appSphinxConfig y)
    results <- executeSSearch mysqlconn s
    [(_ :: T.Text, total :: String)] <- liftIO $ MySQL.query_ mysqlconn "SHOW META LIKE 'total_found'"
    $(logInfoS) "Sphinx" $ T.pack total
    return (toNum total, results)
  where
    toNum t = case readMay t of
        Just x  -> x
        Nothing -> 0

listRandomQuotes :: AuteurId -> Handler [SphinxResult SearchCitation]
listRandomQuotes aid = do
    y <- getYesod
    mysqlconn <- liftIO $ MySQL.connect (myConnInfo $ appSphinxConfig y)
    res <- liftIO $ MySQL.query mysqlconn (MySQL.Query $ E.encodeUtf8 sql) ()
    return res
    where
        sql = T.concat [
            "SELECT texte AS cit, auteurcourt, auteur, oeuvre, reference, titre AS title, titre_unique utitle, 1 AS relevance "
            , "FROM citation"
            , " WHERE ", (T.intercalate " AND " (map toSphinxQL autfilter))
            , " ORDER BY RAND() "
            , " LIMIT 20"
            ]
        autfilter = buildQuoteFilter "auteur_id" aid
