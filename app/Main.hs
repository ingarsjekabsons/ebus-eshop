{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (runAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import DB (migrateAll)
import Database.Esqueleto.Experimental (runMigration)
import Database.Persist.Sqlite (createSqlitePool, runSqlite)
import System.Environment (getEnv)

main :: IO ()
main = do
  runSqlite "esdb.db" $ do
    runMigration migrateAll
  pool <- runStdoutLoggingT $ createSqlitePool "esdb.db" 4
  eshopPort <- getEnv "PORT"
  whURL <- getEnv "WH_URL"
  liftIO $ runAPI pool whURL (read eshopPort)
