{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types
  (
    User (..)
  , UserID
  , UserName
  , Password
  , BindID
  , Service
  , ServiceName
  , CreatedAt
  , Bind (..)
  , Extra
  , TablePrefix
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (Result, convert)

import           Data.Aeson                         (ToJSON (..), Value (..),
                                                     decodeStrict, object, (.=))
import           Data.Hashable                      (Hashable (..))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

type UserID      = Int64
type BindID      = Int64
type UserName    = Text
type Password    = Text
type Service     = Text
type ServiceName = Text
type Extra       = Value
type CreatedAt   = Int64
type TablePrefix = String

data User = User { getUserID        :: UserID
                 , getUserName      :: UserName
                 , getUserPassword  :: Password
                 , getUserExtra     :: Extra
                 , getUserBinds     :: [Bind]
                 , getUserCreatedAt :: CreatedAt
                 }
  deriving (Show)

data Bind = Bind { getBindID        :: BindID
                 , getBindUid       :: UserID
                 , getBindService   :: Service
                 , getBindName      :: ServiceName
                 , getBindExtra     :: Extra
                 , getBindCreatedAt :: CreatedAt
                 }
  deriving (Show)

instance QueryResults User where
  convertResults [fa, fb, fc, fd, fe]
                 [va, vb, vc, vd, ve] = User{..}
    where !getUserID        = convert fa va
          !getUserName      = convert fb vb
          !getUserPassword  = convert fc vc
          !getUserExtra     = convert fd vd
          !getUserBinds     = []
          !getUserCreatedAt = convert fe ve
  convertResults fs vs  = convertError fs vs 2

instance QueryResults Bind where
  convertResults [fa, fb, fc, fd, fe, ff]
                 [va, vb, vc, vd, ve, vf] = Bind{..}
    where !getBindID        = convert fa va
          !getBindUid       = convert fb vb
          !getBindService   = convert fc vc
          !getBindName      = convert fd vd
          !getBindExtra     = convert fe ve
          !getBindCreatedAt = convert ff vf
  convertResults fs vs  = convertError fs vs 2

instance ToJSON User where
  toJSON User{..} = object [ "id"         .= getUserID
                           , "name"       .= getUserName
                           , "extra"      .= getUserExtra
                           , "binds"      .= getUserBinds
                           , "created_at" .= getUserCreatedAt
                           ]

instance ToJSON Bind where
  toJSON Bind{..} = object [ "id"         .= getBindID
                           , "user_id"    .= getBindUid
                           , "name"       .= getBindName
                           , "service"    .= getBindService
                           , "extra"      .= getBindExtra
                           , "created_at" .= getBindCreatedAt
                           ]

instance Result Value where
  convert f (Just bs) = fromMaybe Null (decodeStrict bs)
  convert _ Nothing   = Null
