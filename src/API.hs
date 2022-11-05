{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Control.Exception (throw)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (Product)
import DB
  ( ESProds (..),
    createProd,
    delProd,
    getByName,
    getProd,
    getProds,
    updProd,
    updProdInStock,
  )
import Data.Foldable (for_, traverse_)
import Data.Proxy (Proxy (..))
import Database.Esqueleto.Experimental
  ( ConnectionPool,
    Entity,
    SqlPersistT,
    entityKey,
    entityVal,
    fromSqlKey,
    runSqlPersistM,
    runSqlPersistMPool,
  )
import Deriving.Aeson.Stock (CustomJSON (CustomJSON), FromJSON, PrefixedSnake, ToJSON)
import GHC.Generics
import Integration (WhProduct (..), fetchWHDB, fetchWHProd)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (JSON, errBody)
import Servant.API (Capture, Delete, Get, Patch, Post, ReqBody, type (:<|>) ((:<|>)), type (:>))
import Servant.Server (Handler, Server, err404, err500, serve)

type ESProdsAPI =
  "esprods" :> Get '[JSON] [ESProd]
    :<|> "esprods" :> Capture "esprodId" Integer :> Get '[JSON] ESProd
    :<|> "esprods" :> ReqBody '[JSON] ESProd :> Post '[JSON] ESProd
    :<|> "esprods" :> Capture "esprodId" Integer :> ReqBody '[JSON] ESProd :> Patch '[JSON] ()
    :<|> "esprods" :> Capture "esprodId" Integer :> Delete '[JSON] ()
    :<|> "syncprods" :> Post '[JSON] ()
    :<|> "syncstock" :> Capture "prodId" Integer :> Post '[JSON] ()

esprodsAPI :: Proxy ESProdsAPI
esprodsAPI = Proxy

server :: ConnectionPool -> String -> Server ESProdsAPI
server p wurl =
  apiGetProds p
    :<|> apiGetProd p
    :<|> apiCreateProd p
    :<|> apiUpdProd p
    :<|> apiDelProd p
    :<|> apiSyncProds p wurl
    :<|> apiSyncProdStock p wurl

runAPI :: ConnectionPool -> String -> Int -> IO ()
runAPI pool wurl port = run port (simpleCors $ serve esprodsAPI $ server pool wurl)

apiSyncProdStock :: ConnectionPool -> String -> Integer -> Handler ()
apiSyncProdStock pool wurl pid = do
  liftIO $
    flip runSqlPersistMPool pool $ do
      lp <- getProd pid
      case lp of
        Nothing -> throw err500 {errBody = "Local product not found"}
        Just p -> do
          res <- liftIO $ fetchWHProd ("GET " ++ wurl ++ "/products?name=" ++ eSProdsName (entityVal p))
          case res of
            Nothing -> throw err500 {errBody = "Product not found in WH"}
            Just wp -> updProdInStock (fromIntegral $ fromSqlKey $ entityKey p) (fromIntegral $ productInStock wp)
          return ()
      return ()

apiSyncProds :: ConnectionPool -> String -> Handler ()
apiSyncProds pool wurl = do
  res <- liftIO $ fetchWHDB ("GET " ++ wurl ++ "/products")
  let res' = fmap whProdToProd res
  liftIO $ putStrLn "syncing..."
  liftIO $ putStrLn ("Length of prods: " ++ show (length res))
  liftIO $ forM_ (fmap productName res) print
  liftIO $
    traverse_
      ( \p -> do
          flip runSqlPersistMPool pool $ do
            r <- getByName $ eSProdsName p
            case r of
              Nothing -> do
                liftIO $ putStrLn "Product not found, adding..."
                res <- createProd p
                case res of
                  Nothing -> do
                    liftIO $ putStrLn "Failed to create product"
                    throw $ err500 {errBody = "Failed to create product"}
                  Just en -> do
                    liftIO $ putStrLn "Successfully created product"
                    return ()
                return ()
              Just en -> do
                liftIO $ putStrLn "Product found, doing update"
                let en' = entityVal en
                updProd
                  (fromIntegral $ fromSqlKey $ entityKey en)
                  (eSProdsName p)
                  (eSProdsDescription p)
                  (fromIntegral $ eSProdsInStock p)
                  (fromIntegral $ eSProdsPriceMinorUnits p)
                liftIO $ putStrLn "ProductUpdated"
                return ()
            return ()
      )
      res'
  return ()

whProdToProd :: WhProduct -> ESProds
whProdToProd whp =
  ESProds
    (productName whp)
    (productDescription whp)
    (productInStock whp)
    (productPriceMinorUnits whp)

apiGetProds :: ConnectionPool -> Handler [ESProd]
apiGetProds p = liftIO $ (fmap . fmap) prodToDTO $ flip runSqlPersistMPool p $ do getProds

apiGetProd :: ConnectionPool -> Integer -> Handler ESProd
apiGetProd p i = liftIO $
  flip runSqlPersistMPool p $ do
    prod <- getProd i
    case prod of
      Nothing -> throw $ err404 {errBody = "Product not found"}
      Just prod -> do return $ prodToDTO prod

apiCreateProd :: ConnectionPool -> ESProd -> Handler ESProd
apiCreateProd pool prod = liftIO $
  flip runSqlPersistMPool pool $ do
    res <- createProd $ prodFromDTO prod
    case res of
      Nothing -> throw $ err500 {errBody = "System error"}
      Just p' -> return $ prodToDTO p'

apiUpdProd :: ConnectionPool -> Integer -> ESProd -> Handler ()
apiUpdProd pool pid prod = liftIO $
  flip runSqlPersistMPool pool $ do
    updProd
      (fromIntegral $ prodId prod)
      (prodName prod)
      (prodDescription prod)
      (fromIntegral $ prodInStock prod)
      (fromIntegral $ prodPriceMinorUnits prod)

apiDelProd :: ConnectionPool -> Integer -> Handler ()
apiDelProd pool pid = liftIO $
  flip runSqlPersistMPool pool $ do
    delProd (fromIntegral pid)

data ESProd = ESProd
  { prodId :: Int,
    prodName :: String,
    prodDescription :: String,
    prodInStock :: Int,
    prodPriceMinorUnits :: Int
  }
  deriving stock (Ord, Eq, Generic)
  deriving (ToJSON) via PrefixedSnake "prod" ESProd
  deriving (FromJSON) via PrefixedSnake "prod" ESProd

prodToDTO :: Entity ESProds -> ESProd
prodToDTO p =
  let p' = entityVal p
   in ESProd
        (fromIntegral $ fromSqlKey $ entityKey p)
        (eSProdsName p')
        (eSProdsDescription p')
        (eSProdsInStock p')
        (eSProdsPriceMinorUnits p')

prodFromDTO :: ESProd -> ESProds
prodFromDTO p =
  ESProds
    (prodName p)
    (prodDescription p)
    (prodInStock p)
    (prodPriceMinorUnits p)