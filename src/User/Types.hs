{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module User.Types
  ( User (..)
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

import           Data.Aeson                         (FromJSON (..), ToJSON (..),
                                                     Value (..), object,
                                                     withObject, (.:), (.=))
import           Data.Int                           (Int64)
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           GHC.Generics                       (Generic)
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

data User = User
    { getUserID          :: UserID
    , getUserName        :: UserName
    , getUserPassword    :: Password
    , getUserExtra       :: Extra
    , getUserSecureExtra :: Extra
    , getUserBinds       :: [Bind]
    , getUserGroups      :: [GroupName]
    , getUserCreatedAt   :: CreatedAt
    }
    deriving (Show)

instance FromRow User where
  fromRow = do
    getUserID <- field
    getUserName <- field
    getUserPassword <- field
    getUserExtra <- field
    getUserSecureExtra <- field
    getUserCreatedAt <- field
    return User
      { getUserBinds = []
      , getUserGroups = []
      , ..
      }

data Bind = Bind
    { getBindID        :: BindID
    , getBindUid       :: UserID
    , getBindService   :: Service
    , getBindName      :: ServiceName
    , getBindExtra     :: Extra
    , getBindCreatedAt :: CreatedAt
    }
    deriving (Show, Generic, FromRow)


data GroupMeta = GroupMeta
    { getGroup          :: GroupName
    , getGroupTitle     :: GroupTitle
    , getGroupSummary   :: GroupSummary
    , getGroupUserCount :: Int64
    , getGroupCreatedAt :: CreatedAt
    }
    deriving (Show)


instance FromRow GroupMeta where
  fromRow = do
    getGroup <- field
    getGroupTitle <- field
    getGroupSummary <- field
    getGroupCreatedAt <- field
    return GroupMeta
      { getGroupUserCount = 0
      , ..
      }


instance ToJSON User where
  toJSON User{..} = object
    [ "id"           .= getUserID
    , "name"         .= getUserName
    , "password"     .= getUserPassword
    , "extra"        .= getUserExtra
    , "secure_extra" .= getUserSecureExtra
    , "binds"        .= getUserBinds
    , "groups"       .= getUserGroups
    , "created_at"   .= getUserCreatedAt
    ]

instance ToJSON Bind where
  toJSON Bind{..} = object
    [ "id"         .= getBindID
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
    [ "id"         .= getOUserID
    , "name"       .= getOUserName
    , "extra"      .= getOUserExtra
    , "binds"      .= getOUserBinds
    , "groups"     .= getOUserGroups
    , "created_at" .= getOUserCreatedAt
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

instance FromJSON GroupMeta where
  parseJSON = withObject "GroupMeta" $ \o -> do
    getGroup <- o .: "group"
    getGroupTitle <- o .: "title"
    getGroupSummary <- o .: "summary"
    getGroupUserCount <- o .: "user_count"
    getGroupCreatedAt <- o .: "created_at"
    return GroupMeta{..}
