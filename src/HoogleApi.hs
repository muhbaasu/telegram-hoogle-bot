{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HoogleApi where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Client

data HoogleQuery = HoogleQuery {
    version :: Text
  , results :: [HoogleResult]
} deriving (FromJSON, Show, Generic)

data HoogleResult = HoogleResult {
    location :: Text
  , self :: Text
  , docs :: Text
} deriving (FromJSON, Show, Generic)

type HoogleAPI = "hoogle"
              :> QueryParam "mode" Text
              :> QueryParam "hoogle" Text
              :> QueryParam "start" Int
              :> QueryParam "count" Int
              :> Get '[JSON] HoogleQuery

api :: Proxy HoogleAPI
api = Proxy

query_ :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> EitherT ServantError IO HoogleQuery
query_ = client api (BaseUrl Https "haskell.org" 443)

query :: Text -> Maybe Int -> Maybe Int -> IO (Either ServantError HoogleQuery)
query hoogle start count =
  let mode = Just "json"
      hoogle' = Just hoogle
      start' = Just $ fromMaybe 1 start
      count' = Just $ fromMaybe 10 count
      in runEitherT $ query_ mode hoogle' start' count'

