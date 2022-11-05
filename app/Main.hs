{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (runAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import DB (migrateAll)
import Database.Esqueleto.Experimental (runMigration)
import Database.Persist.Sqlite (createSqlitePool, runSqlite)

main :: IO ()
main = do
  runSqlite "esdb.db" $ do
    runMigration migrateAll
  pool <- runStdoutLoggingT $ createSqlitePool "esdb.db" 4
  liftIO $ runAPI pool "http://127.0.0.1:8888" 9999
