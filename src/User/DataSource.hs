{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module User.DataSource (
    UserReq(..),
    initUserState
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           User.DataSource.Bind
import           User.DataSource.Group
import           User.DataSource.Table
import           User.DataSource.User
import           User.Types
import           Yuntan.Types.HasMySQL    (HasMySQL, MySQL, mysqlPool,
                                           tablePrefix)
import           Yuntan.Types.ListResult  (From, Size)
import           Yuntan.Types.OrderBy     (OrderBy)

import qualified Control.Exception        (SomeException, bracket_, try)
import           Data.Int                 (Int64)
import           Data.Pool                (withResource)

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
  CountBindByUID     :: UserID -> UserReq Int64
  GetBindListByUID   :: UserID -> UserReq [Bind]
  RemoveBindByUID    :: UserID -> UserReq Int64

  MergeData          :: UserReq ()

  AddGroup                :: GroupName -> UserID -> UserReq ()
  RemoveGroup             :: String -> UserID -> UserReq Int64
  GetGroupListByUserID    :: UserID -> UserReq [GroupName]
  GetUserIDListByGroup    :: GroupName -> From -> Size -> OrderBy -> UserReq [UserID]
  RemoveGroupListByUserID :: UserID -> UserReq Int64
  CountGroup              :: GroupName -> UserReq Int64

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
  hashWithSalt s (CountBindByUID uid)     = hashWithSalt s (15::Int, uid)
  hashWithSalt s (GetBindListByUID uid)   = hashWithSalt s (16::Int, uid)
  hashWithSalt s (RemoveBindByUID uid)    = hashWithSalt s (17::Int, uid)

  hashWithSalt s MergeData                = hashWithSalt s (20::Int)

  hashWithSalt s (AddGroup n uid)                = hashWithSalt s (21::Int, n, uid)
  hashWithSalt s (RemoveGroup n uid)             = hashWithSalt s (22::Int, n, uid)
  hashWithSalt s (GetGroupListByUserID uid)      = hashWithSalt s (23::Int, uid)
  hashWithSalt s (GetUserIDListByGroup n f si o) = hashWithSalt s (24::Int, n, f, si, o)
  hashWithSalt s (RemoveGroupListByUserID uid)   = hashWithSalt s (25::Int, uid)
  hashWithSalt s (CountGroup n)                  = hashWithSalt s (26::Int, n)

deriving instance Show (UserReq a)
instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = UserState { numThreads :: Int }

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance HasMySQL u => DataSource u UserReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State UserReq
  -> Flags
  -> u
  -> [BlockedFetch UserReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch UserReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch UserReq -> MySQL ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: UserReq a -> MySQL a
fetchReq  (CreateUser n h)              = createUser n h
fetchReq  (GetUser k)                   = getUser k
fetchReq  (GetUserByName k)             = getUserByName k
fetchReq  (RemoveUser k)                = removeUser k
fetchReq  (UpdateUserName k s)          = updateUserName k s
fetchReq  (UpdateUserPassword k p)      = updateUserPassword k  p
fetchReq  (UpdateUserExtra k e)         = updateUserExtra k e
fetchReq  (GetUsers f s o)              = getUsers f s o
fetchReq  CountUser                     = countUser

fetchReq (CreateBind uid se n ex)       = createBind uid se n ex
fetchReq (GetBind bid)                  = getBind bid
fetchReq (GetBindByName n)              = getBindByName n
fetchReq (RemoveBind bid)               = removeBind bid
fetchReq (UpdateBindExtra bid ex)       = updateBindExtra bid ex
fetchReq (CountBindByUID uid)           = countBindByUID uid
fetchReq (GetBindListByUID uid)         = getBindListByUID uid
fetchReq (RemoveBindByUID uid)          = removeBindByUID uid

fetchReq MergeData                      = mergeData

fetchReq (AddGroup n uid)               = addGroup n uid
fetchReq (RemoveGroup n uid)            = removeGroup n uid
fetchReq (GetGroupListByUserID uid)     = getGroupListByUserID uid
fetchReq (GetUserIDListByGroup n f s o) = getUserIDListByGroup n f s o
fetchReq (RemoveGroupListByUserID uid)  = removeGroupListByUserID uid
fetchReq (CountGroup n)                 = countGroup n


initUserState :: Int -> State UserReq
initUserState = UserState
