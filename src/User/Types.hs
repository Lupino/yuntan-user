{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module User.Types
  (
    User (..)
  , UserID
  , UserName
  , Password
  , BindID
  , GroupName
  , Service
  , ServiceName
  , CreatedAt
  , Bind (..)
  , Extra
  , TablePrefix
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..), Value (..),
                                                     decodeStrict, object, (.=))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Yuntan.Utils.JSON                  (unionValue)

type UserID      = Int64
type BindID      = Int64
type UserName    = Text
type Password    = Text
type Service     = Text
type ServiceName = Text
type GroupName   = Text
type Extra       = Value
type CreatedAt   = Int64
type TablePrefix = String

data User = User { getUserID          :: UserID
                 , getUserName        :: UserName
                 , getUserPassword    :: Password
                 , getUserExtra       :: Extra
                 , getUserSecureExtra :: Extra
                 , getUserBinds       :: [Bind]
                 , getUserGroups      :: [GroupName]
                 , getUserCreatedAt   :: CreatedAt
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
  convertResults [fa, fb, fc, _,  fe,  _]
                 [va, vb, vc, vd, ve, vf] = User{..}
    where !getUserID          = convert fa va
          !getUserName        = convert fb vb
          !getUserPassword    = convert fc vc
          !getUserExtra       = fromMaybe Null . decodeStrict $ fromMaybe "{}" vd
          !getUserBinds       = []
          !getUserGroups      = []
          !getUserCreatedAt   = convert fe ve
          !getUserSecureExtra = fromMaybe Null . decodeStrict $ fromMaybe "{}" vf
  convertResults fs vs  = convertError fs vs 2

instance QueryResults Bind where
  convertResults [fa, fb, fc, fd, _, ff]
                 [va, vb, vc, vd, ve, vf] = Bind{..}
    where !getBindID        = convert fa va
          !getBindUid       = convert fb vb
          !getBindService   = convert fc vc
          !getBindName      = convert fd vd
          !getBindExtra     = fromMaybe Null . decodeStrict $ fromMaybe "{}" ve
          !getBindCreatedAt = convert ff vf
  convertResults fs vs  = convertError fs vs 2

instance ToJSON User where
  toJSON User{..} = object [ "id"         .= getUserID
                           , "name"       .= getUserName
                           , "extra"      .= unionValue getUserSecureExtra getUserExtra
                           , "binds"      .= getUserBinds
                           , "groups"     .= getUserGroups
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
