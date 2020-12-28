{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , runDB
    , Spec
    , writeLn
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)

import Foundation
import Model

import System.IO

writeLn :: String -> YesodExample site ()
writeLn = liftIO . (hPutStrLn stderr)

type Spec = YesodSpec App

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool
