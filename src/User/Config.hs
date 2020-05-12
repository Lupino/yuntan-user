{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module User.Config
  (
    PSQLConfig (..)
  , Config (..)
  , genPSQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , redisEnv
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Database.Redis            (Connection)
import           Yuntan.Config.PSQLConfig  (PSQLConfig (..), genPSQLPool)
import           Yuntan.Config.RedisConfig (RedisConfig (..),
                                            defaultRedisConfig,
                                            genRedisConnection)
import           Yuntan.Types.HasPSQL      (HasOtherEnv, otherEnv)

data Config = Config
    { psqlConfig  :: PSQLConfig
    , redisConfig :: RedisConfig
    }
    deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig  <- o .: "psql"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    return Config{..}

newtype Cache = Cache
  { redis :: Maybe Connection
  }

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: Maybe Connection -> Cache
mkCache = Cache
