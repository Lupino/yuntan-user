{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module User.DataSource (
    UserReq(..),
    initGlobalState
  ) where

import           Data.Hashable             (Hashable (..))
import           Data.Typeable             (Typeable)
import           Haxl.Core                 (BlockedFetch (..), DataSource,
                                            DataSourceName, Flags,
                                            PerformFetch (..), ShowP, State,
                                            StateKey, StateStore,
                                            dataSourceName, fetch, putFailure,
                                            putSuccess, showp, stateEmpty,
                                            stateSet)

import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)
import           User.DataSource.Bind
import           User.DataSource.Table
import           User.DataSource.User
import           User.Types
import           User.UserEnv              (UserEnv (..))

import qualified Control.Exception         (SomeException, bracket_, try)
import           Data.Int                  (Int64)
import           Data.Pool                 (withResource)
import           Database.MySQL.Simple     (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data UserReq a where
  CreateUser         :: UserName -> Password -> UserReq UserID
  GetUser            :: UserID -> UserReq (Maybe User)
  GetUserByName      :: UserName -> UserReq (Maybe User)
  RemoveUser         :: UserID -> UserReq Int64
  UpdateUserName     :: UserID -> UserName -> UserReq Int64
  UpdateUserPassword :: UserID -> Password -> UserReq Int64
  UpdateUserExtra    :: UserID -> Extra -> UserReq Int64
  GetUsers           :: From -> Size -> OrderBy -> UserReq [User]
  CountUser          :: UserReq Int64

  CreateBind         :: UserID -> Service -> ServiceName -> Extra -> UserReq BindID
  GetBind            :: BindID -> UserReq (Maybe Bind)
  GetBindByName      :: ServiceName -> UserReq (Maybe Bind)
  RemoveBind         :: BindID -> UserReq Int64
  UpdateBindExtra    :: BindID -> Extra -> UserReq Int64
  CountBind          :: UserID -> UserReq Int64
  GetBinds           :: UserID -> UserReq [Bind]
  RemoveBinds        :: UserID -> UserReq Int64

  CreateTable        :: UserReq Int64

  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
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

deriving instance Show (UserReq a)
instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = UserState { numThreads :: Int }

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource UserEnv UserReq where
  fetch = dispatchFetch

dispatchFetch
  :: State UserReq
  -> Flags
  -> UserEnv
  -> [BlockedFetch UserReq]
  -> PerformFetch

dispatchFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> UserEnv -> BlockedFetch UserReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mySQLPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch UserReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: UserReq a -> TablePrefix -> Connection -> IO a
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
  where dispatchState = UserState threads
