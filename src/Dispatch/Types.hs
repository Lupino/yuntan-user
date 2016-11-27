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
  , From
  , Size
  , OrderBy
  , asc
  , desc
  , emptyOrder
  , Extra
  , unionExtra
  , differenceExtra
  , TablePrefix
  ) where

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (Result, convert)

import           Data.Aeson                         (ToJSON (..), Value (..),
                                                     decodeStrict, object, (.=))
import           Data.Hashable                      (Hashable (..))
import           Data.HashMap.Strict                (difference, union)
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)

type UserID      = Int64
type BindID      = Int64
type From        = Int64
type Size        = Int64
type UserName    = Text
type Password    = Text
type Service     = Text
type ServiceName = Text
type Extra       = Value
type CreatedAt   = Int64
type TablePrefix = String

data OrderBy = Desc String | Asc String | EmptyOrder
  deriving (Generic, Eq)

instance Hashable OrderBy

desc :: String -> OrderBy
desc = Desc

asc :: String -> OrderBy
asc = Asc

emptyOrder :: OrderBy
emptyOrder = EmptyOrder

quote :: String -> String
quote s@(x:xs) | '.' `elem` s = x : quote xs
               | otherwise    = '`' : x : xs ++ "`"
quote []                      = []

instance Show OrderBy where
  show (Desc field) = "ORDER BY " ++ quote field ++ " DESC"
  show (Asc field)  = "ORDER BY " ++ quote field ++ " ASC"
  show EmptyOrder   = ""

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

unionExtra :: Value -> Value -> Value
unionExtra (Object a) (Object b) = Object $ union a b
unionExtra (Object a) _          = Object a
unionExtra _ (Object b)          = Object b
unionExtra _ _                   = Null

differenceExtra :: Value -> Value -> Value
differenceExtra (Object a) (Object b) = Object $ difference a b
differenceExtra (Object a) _          = Object a
differenceExtra _ _                   = Null
