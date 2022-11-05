{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB
  ( getProds,
    getProd,
    getByName,
    updProd,
    updProdInStock,
    delProd,
    createProd,
    ESProds (..),
    migrateAll,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Database.Esqueleto.Experimental
  ( Entity (entityVal),
    SqlBackend,
    SqlPersistT,
    delete,
    from,
    insertUniqueEntity,
    select,
    selectOne,
    set,
    update,
    upsert,
    val,
    valkey,
    where_,
    (=.),
    (==.),
    (^.),
  )
import Database.Esqueleto.Experimental.From (table)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
        ESProds
          name String
          description String
          inStock Int
          priceMinorUnits Int
          UniqueESProd name
          deriving Eq Show
    |]

getProds :: MonadIO m => SqlPersistT m [Entity ESProds]
getProds = do
  select $ from $ table @ESProds

getProd :: MonadIO m => Integer -> SqlPersistT m (Maybe (Entity ESProds))
getProd pid = do
  selectOne $ do
    prod <- from $ table @ESProds
    where_ (prod ^. ESProdsId ==. valkey (fromIntegral pid))
    return prod

getByName :: MonadIO m => String -> SqlPersistT m (Maybe (Entity ESProds))
getByName name = do
  selectOne $ do
    prod <- from $ table @ESProds
    where_ (prod ^. ESProdsName ==. val name)
    return prod

delProd :: MonadIO m => Int -> SqlPersistT m ()
delProd pid = do
  delete $ do
    prod <- from $ table @ESProds
    where_ (prod ^. ESProdsId ==. valkey (fromIntegral pid))

updProd :: MonadIO m => Integer -> String -> String -> Integer -> Integer -> SqlPersistT m ()
updProd pid name descr count price = do
  update $ \p -> do
    set p [ESProdsName =. val name, ESProdsDescription =. val descr, ESProdsInStock =. val (fromIntegral count), ESProdsPriceMinorUnits =. val (fromIntegral price)]
    where_ (p ^. ESProdsId ==. valkey (fromIntegral pid))

updProdInStock :: MonadIO m => Integer -> Integer -> SqlPersistT m ()
updProdInStock pid stock = do
  update $ \p -> do
    set p [ESProdsInStock =. val (fromIntegral stock)]
    where_ (p ^. ESProdsId ==. valkey (fromIntegral pid))

createProd :: MonadIO m => ESProds -> SqlPersistT m (Maybe (Entity ESProds))
createProd = insertUniqueEntity