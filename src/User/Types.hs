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
  , GroupTitle
  , GroupSummary
  , GroupMeta (..)
  , Service
  , ServiceName
  , CreatedAt
  , Bind (..)
  , Extra
  , TablePrefix
  , OUser
  , toOUser
  , toOUser'
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     Value (..), decodeStrict,
                                                     object, withObject, (.:),
                                                     (.=))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Yuntan.Utils.JSON                  (unionValue)

type UserID       = Int64
type BindID       = Int64
type UserName     = Text
type Password     = Text
type Service      = Text
type ServiceName  = Text
type GroupName    = Text
type GroupTitle   = String
type GroupSummary = String
type Extra        = Value
type CreatedAt    = Int64
type TablePrefix  = String

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

data GroupMeta = GroupMeta
  { getGroup          :: GroupName
  , getGroupTitle     :: GroupTitle
  , getGroupSummary   :: GroupSummary
  , getGroupUserCount :: Int64
  , getGroupCreatedAt :: CreatedAt
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

instance QueryResults GroupMeta where
  convertResults [fa, fb, fc, fd]
                 [va, vb, vc, vd] = GroupMeta{..}
    where !getGroup          = convert fa va
          !getGroupTitle     = convert fb vb
          !getGroupSummary   = convert fc vc
          !getGroupUserCount = 0
          !getGroupCreatedAt = convert fd vd
  convertResults fs vs  = convertError fs vs 2

instance ToJSON User where
  toJSON User{..} = object [ "id"           .= getUserID
                           , "name"         .= getUserName
                           , "password"     .= getUserPassword
                           , "extra"        .= getUserExtra
                           , "secure_extra" .= getUserSecureExtra
                           , "binds"        .= getUserBinds
                           , "groups"       .= getUserGroups
                           , "created_at"   .= getUserCreatedAt
                           ]

instance ToJSON Bind where
  toJSON Bind{..} = object [ "id"         .= getBindID
                           , "user_id"    .= getBindUid
                           , "name"       .= getBindName
                           , "service"    .= getBindService
                           , "extra"      .= getBindExtra
                           , "created_at" .= getBindCreatedAt
                           ]

instance ToJSON GroupMeta where
  toJSON GroupMeta{..} = object
    [ "group"      .= getGroup
    , "title"      .= getGroupTitle
    , "summary"    .= getGroupSummary
    , "user_count" .= getGroupUserCount
    , "created_at" .= getGroupCreatedAt
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    getUserID          <- o .: "id"
    getUserName        <- o .: "name"
    getUserPassword    <- o .: "password"
    getUserExtra       <- o .: "extra"
    getUserSecureExtra <- o .: "secure_extra"
    getUserBinds       <- o .: "binds"
    getUserGroups      <- o .: "groups"
    getUserCreatedAt   <- o .: "created_at"
    return User{..}

instance FromJSON Bind where
  parseJSON = withObject "Bind" $ \o -> do
    getBindID <- o .: "id"
    getBindUid <- o .: "user_id"
    getBindName <- o .: "name"
    getBindService <- o .: "service"
    getBindExtra <- o .: "extra"
    getBindCreatedAt <- o .: "created_at"
    return Bind{..}

data OUser = OUser
  { getOUserID        :: UserID
  , getOUserName      :: UserName
  , getOUserExtra     :: Extra
  , getOUserBinds     :: [Bind]
  , getOUserGroups    :: [GroupName]
  , getOUserCreatedAt :: CreatedAt
  }
  deriving (Show)

instance ToJSON OUser where
  toJSON OUser{..} = object
    [ "id"           .= getOUserID
    , "name"         .= getOUserName
    , "extra"        .= getOUserExtra
    , "binds"        .= getOUserBinds
    , "groups"       .= getOUserGroups
    , "created_at"   .= getOUserCreatedAt
    ]

toOUser :: User -> OUser
toOUser User{..} = OUser
  { getOUserID        = getUserID
  , getOUserName      = getUserName
  , getOUserExtra     = unionValue getUserSecureExtra getUserExtra
  , getOUserBinds     = getUserBinds
  , getOUserGroups    = getUserGroups
  , getOUserCreatedAt = getUserCreatedAt
  }

toOUser' :: Maybe User -> Maybe OUser
toOUser' = fmap toOUser
