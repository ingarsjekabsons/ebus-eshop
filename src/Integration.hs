{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE StrictData #-}

module Integration (fetchWHDB, fetchWHProd, WhProduct (..)) where

import Data.Aeson (FromJSON, ToJSON, decode, decodeStrict)
import Deriving.Aeson.Stock (CustomJSON (CustomJSON), FromJSON, PrefixedSnake, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Response (responseBody),
    brRead,
    defaultManagerSettings,
    newManager,
    parseRequest,
    withResponse,
  )
import Network.HTTP.Client.TLS (newTlsManager)

fetchWHDB :: String -> IO [WhProduct]
fetchWHDB url = do
  manager <- newTlsManager
  req <- parseRequest url
  withResponse
    req
    manager
    ( \resp -> do
        bs <- brRead $ responseBody resp
        let p = decodeStrict bs :: Maybe [WhProduct]
        case p of
          Nothing -> return []
          Just wps -> return wps
    )

fetchWHProd :: String -> IO (Maybe WhProduct)
fetchWHProd url = do
  manager <- newTlsManager
  req <- parseRequest url
  withResponse
    req
    manager
    ( \resp -> do
        bs <- brRead $ responseBody resp
        let prods = decodeStrict bs :: Maybe [WhProduct]
        case prods of
          Nothing -> return Nothing
          Just pps -> do
            if not (null pps)
              then return $ Just $ head pps
              else return Nothing
    )

data WhProduct = WhProduct
  { productId :: Int,
    productName :: String,
    productDescription :: String,
    productInStock :: Int,
    productPriceMinorUnits :: Int
  }
  deriving stock (Ord, Eq, Generic)
  deriving (ToJSON) via PrefixedSnake "product" WhProduct
  deriving (FromJSON) via PrefixedSnake "product" WhProduct
