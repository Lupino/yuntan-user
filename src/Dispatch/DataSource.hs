{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Dispatch.DataSource (
    DispatchReq(..),
    initGlobalState
  ) where

import           Data.Hashable             (Hashable (..))
import           Data.Typeable             (Typeable)
import           Haxl.Core                 (BlockedFetch (..), DataSource,
                                            DataSourceName, Flags,
                                            PerformFetch (..), Show1, State,
                                            StateKey, StateStore,
                                            dataSourceName, fetch, putFailure,
                                            putSuccess, show1, stateEmpty,
                                            stateSet)

import           Dispatch.DataSource.Bind
import           Dispatch.DataSource.Table
import           Dispatch.DataSource.User
import           Dispatch.Types
import           Dispatch.UserEnv          (UserEnv (..))

import qualified Control.Exception         (SomeException, bracket_, try)
import           Data.Int                  (Int64)
import           Data.Pool                 (Pool, withResource)
import           Database.MySQL.Simple     (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import           Data.Maybe                (fromJust, isJust)

-- Data source implementation.

data DispatchReq a where
  CreateUser         :: UserName -> Password -> DispatchReq UserID
  GetUser            :: UserID -> DispatchReq (Maybe User)
  GetUserByName      :: UserName -> DispatchReq (Maybe User)
  RemoveUser         :: UserID -> DispatchReq Int64
  UpdateUserName     :: UserID -> UserName -> DispatchReq Int64
  UpdateUserPassword :: UserID -> Password -> DispatchReq Int64
  UpdateUserExtra    :: UserID -> Extra -> DispatchReq Int64
  GetUsers           :: From -> Size -> OrderBy -> DispatchReq [User]
  CountUser          :: DispatchReq Int64

  CreateBind         :: UserID -> Service -> ServiceName -> Extra -> DispatchReq BindID
  GetBind            :: BindID -> DispatchReq (Maybe Bind)
  GetBindByName      :: ServiceName -> DispatchReq (Maybe Bind)
  RemoveBind         :: BindID -> DispatchReq Int64
  UpdateBindExtra    :: BindID -> Extra -> DispatchReq Int64
  CountBind          :: UserID -> DispatchReq Int64
  GetBinds           :: UserID -> DispatchReq [Bind]
  RemoveBinds        :: UserID -> DispatchReq Int64

  CreateTable        :: DispatchReq Int64

  deriving (Typeable)

deriving instance Eq (DispatchReq a)
instance Hashable (DispatchReq a) where
  hashWithSalt s (CreateUser n h)         = hashWithSalt s (0::Int, n, h)
  hashWithSalt s (GetUser k)              = hashWithSalt s (1::Int, k)
  hashWithSalt s (GetUserByName k)        = hashWithSalt s (2::Int, k)
  hashWithSalt s (RemoveUser k)           = hashWithSalt s (3::Int, k)
  hashWithSalt s (UpdateUserName k n)     = hashWithSalt s (4::Int, k, n)
  hashWithSalt s (UpdateUserPassword k p) = hashWithSalt s (5::Int, k, p)
  hashWithSalt s (UpdateUserExtra k ex)   = hashWithSalt s (6::Int, k, ex)
  hashWithSalt s (GetUsers f si o)        = hashWithSalt s (7::Int, f, si, o)
  hashWithSalt s CountUser                = hashWithSalt s (8::Int)

  hashWithSalt s (CreateBind uid se n ex) = hashWithSalt s (10::Int, uid, se, n, ex)
  hashWithSalt s (GetBind bid)            = hashWithSalt s (11::Int, bid)
  hashWithSalt s (GetBindByName n)        = hashWithSalt s (12::Int, n)
  hashWithSalt s (RemoveBind bid)         = hashWithSalt s (13::Int, bid)
  hashWithSalt s (UpdateBindExtra bid ex) = hashWithSalt s (14::Int, bid, ex)
  hashWithSalt s (CountBind uid)          = hashWithSalt s (15::Int, uid)
  hashWithSalt s (GetBinds uid)           = hashWithSalt s (16::Int, uid)
  hashWithSalt s (RemoveBinds uid)        = hashWithSalt s (17::Int, uid)

  hashWithSalt s CreateTable              = hashWithSalt s (20::Int)

deriving instance Show (DispatchReq a)
instance Show1 DispatchReq where show1 = show

instance StateKey DispatchReq where
  data State DispatchReq = DispatchState { numThreads :: Int }

instance DataSourceName DispatchReq where
  dataSourceName _ = "DispatchDataSource"

instance DataSource UserEnv DispatchReq where
  fetch = dispatchFetch

dispatchFetch
  :: State DispatchReq
  -> Flags
  -> UserEnv
  -> [BlockedFetch DispatchReq]
  -> PerformFetch

dispatchFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> UserEnv -> BlockedFetch DispatchReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mySQLPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch DispatchReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: DispatchReq a -> TablePrefix -> Connection -> IO a
fetchReq  (CreateUser n h)         = createUser n h
fetchReq  (GetUser k)              = getUser k
fetchReq  (GetUserByName k)        = getUserByName k
fetchReq  (RemoveUser k)           = removeUser k
fetchReq  (UpdateUserName k s)     = updateUserName k s
fetchReq  (UpdateUserPassword k p) = updateUserPassword k  p
fetchReq  (UpdateUserExtra k e)    = updateUserExtra k e
fetchReq  (GetUsers f s o)         = getUsers f s o
fetchReq  CountUser                = countUser

fetchReq (CreateBind uid se n ex)  = createBind uid se n ex
fetchReq (GetBind bid)             = getBind bid
fetchReq (GetBindByName n)         = getBindByName n
fetchReq (RemoveBind bid)          = removeBind bid
fetchReq (UpdateBindExtra bid ex)  = updateBindExtra bid ex
fetchReq (CountBind uid)           = countBind uid
fetchReq (GetBinds uid)            = getBinds uid
fetchReq (RemoveBinds uid)         = removeBinds uid

fetchReq CreateTable               = createTable




initGlobalState :: Int -> StateStore
initGlobalState threads = stateSet dispatchState stateEmpty
  where dispatchState = DispatchState threads
