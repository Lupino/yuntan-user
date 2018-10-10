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
  GetUserIdByName    :: UserName -> UserReq (Maybe UserID)
  RemoveUser         :: UserID -> UserReq Int64
  UpdateUserName     :: UserID -> UserName -> UserReq Int64
  UpdateUserPassword :: UserID -> Password -> UserReq Int64
  UpdateUserExtra    :: UserID -> Extra -> UserReq Int64
  UpdateUserSecureExtra :: UserID -> Extra -> UserReq Int64
  GetUserIdList      :: From -> Size -> OrderBy -> UserReq [UserID]
  CountUser          :: UserReq Int64

  CreateBind         :: UserID -> Service -> ServiceName -> Extra -> UserReq BindID
  GetBind            :: BindID -> UserReq (Maybe Bind)
  GetBindIdByName      :: ServiceName -> UserReq (Maybe BindID)
  RemoveBind         :: BindID -> UserReq Int64
  UpdateBindExtra    :: BindID -> Extra -> UserReq Int64
  CountBindByUID     :: UserID -> UserReq Int64
  GetBindIdListByUID   :: UserID -> From -> Size -> OrderBy -> UserReq [BindID]
  RemoveBindByUID    :: UserID -> UserReq Int64
  CountBindByService :: Service -> UserReq Int64
  GetBindIdListByService :: Service -> From -> Size -> OrderBy -> UserReq [BindID]
  CountBindByUIDAndService :: UserID -> Service -> UserReq Int64
  GetBindIdListByUIDAndService :: UserID -> Service -> From -> Size -> OrderBy -> UserReq [BindID]

  MergeData          :: UserReq ()

  AddGroup                :: GroupName -> UserID -> UserReq ()
  RemoveGroup             :: GroupName -> UserID -> UserReq Int64
  GetGroupListByUserId    :: UserID -> UserReq [GroupName]
  GetUserIdListByGroup    :: GroupName -> From -> Size -> OrderBy -> UserReq [UserID]
  RemoveGroupListByUserId :: UserID -> UserReq Int64
  CountGroup              :: GroupName -> UserReq Int64

  SaveGroupMeta    :: GroupName -> GroupTitle -> GroupSummary -> UserReq Int64
  GetGroupMeta     :: GroupName -> UserReq (Maybe GroupMeta)
  RemoveGroupMeta  :: GroupName -> UserReq Int64
  GetGroupMetaList :: UserReq [GroupMeta]

  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
  hashWithSalt s (CreateUser n h)                           = hashWithSalt s (0::Int, n, h)
  hashWithSalt s (GetUser k)                                = hashWithSalt s (1::Int, k)
  hashWithSalt s (GetUserIdByName k)                        = hashWithSalt s (2::Int, k)
  hashWithSalt s (RemoveUser k)                             = hashWithSalt s (3::Int, k)
  hashWithSalt s (UpdateUserName k n)                       = hashWithSalt s (4::Int, k, n)
  hashWithSalt s (UpdateUserPassword k p)                   = hashWithSalt s (5::Int, k, p)
  hashWithSalt s (UpdateUserExtra k ex)                     = hashWithSalt s (6::Int, k, ex)
  hashWithSalt s (UpdateUserSecureExtra k ex)               = hashWithSalt s (7::Int, k, ex)
  hashWithSalt s (GetUserIdList f si o)                     = hashWithSalt s (8::Int, f, si, o)
  hashWithSalt s CountUser                                  = hashWithSalt s (9::Int)

  hashWithSalt s (CreateBind uid se n ex)                   = hashWithSalt s (10::Int, uid, se, n, ex)
  hashWithSalt s (GetBind bid)                              = hashWithSalt s (11::Int, bid)
  hashWithSalt s (GetBindIdByName n)                          = hashWithSalt s (12::Int, n)
  hashWithSalt s (RemoveBind bid)                           = hashWithSalt s (13::Int, bid)
  hashWithSalt s (UpdateBindExtra bid ex)                   = hashWithSalt s (14::Int, bid, ex)
  hashWithSalt s (CountBindByUID uid)                       = hashWithSalt s (15::Int, uid)
  hashWithSalt s (GetBindIdListByUID uid f i o)               = hashWithSalt s (16::Int, uid, f, i, o)
  hashWithSalt s (CountBindByService srv)                   = hashWithSalt s (17::Int, srv)
  hashWithSalt s (GetBindIdListByService srv f i o)           = hashWithSalt s (18::Int, srv, f, i, o)
  hashWithSalt s (CountBindByUIDAndService uid srv)         = hashWithSalt s (19::Int, uid, srv)
  hashWithSalt s (GetBindIdListByUIDAndService uid srv f i o) = hashWithSalt s (20::Int, uid, srv, f, i, o)
  hashWithSalt s (RemoveBindByUID uid)                      = hashWithSalt s (21::Int, uid)

  hashWithSalt s MergeData                                  = hashWithSalt s (22::Int)

  hashWithSalt s (AddGroup n uid)                           = hashWithSalt s (23::Int, n, uid)
  hashWithSalt s (RemoveGroup n uid)                        = hashWithSalt s (24::Int, n, uid)
  hashWithSalt s (GetGroupListByUserId uid)                 = hashWithSalt s (25::Int, uid)
  hashWithSalt s (GetUserIdListByGroup n f si o)            = hashWithSalt s (26::Int, n, f, si, o)
  hashWithSalt s (RemoveGroupListByUserId uid)              = hashWithSalt s (27::Int, uid)
  hashWithSalt s (CountGroup n)                             = hashWithSalt s (28::Int, n)

  hashWithSalt s (SaveGroupMeta n t c)                      = hashWithSalt s (29::Int, n, t, c)
  hashWithSalt s (GetGroupMeta n)                           = hashWithSalt s (30::Int, n)
  hashWithSalt s (RemoveGroupMeta n)                        = hashWithSalt s (31::Int, n)
  hashWithSalt s GetGroupMetaList                           = hashWithSalt s (32::Int)

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
  -> PerformFetch UserReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
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
fetchReq  (CreateUser n h)                = createUser n h
fetchReq  (GetUser k)                     = getUser k
fetchReq  (GetUserIdByName k)             = getUserIdByName k
fetchReq  (RemoveUser k)                  = removeUser k
fetchReq  (UpdateUserName k s)            = updateUserName k s
fetchReq  (UpdateUserPassword k p)        = updateUserPassword k  p
fetchReq  (UpdateUserExtra k e)           = updateUserExtra k e
fetchReq  (UpdateUserSecureExtra k e)     = updateUserSecureExtra k e
fetchReq  (GetUserIdList f s o)           = getUserIdList f s o
fetchReq  CountUser                       = countUser

fetchReq (CreateBind uid se n ex)         = createBind uid se n ex
fetchReq (GetBind bid)                    = getBind bid
fetchReq (GetBindIdByName n)                = getBindIdByName n
fetchReq (RemoveBind bid)                 = removeBind bid
fetchReq (UpdateBindExtra bid ex)         = updateBindExtra bid ex
fetchReq (CountBindByUID uid)             = countBindByUID uid
fetchReq (GetBindIdListByUID uid f s o)     = getBindIdListByUID uid f s o
fetchReq (CountBindByService srv)         = countBindByService srv
fetchReq (GetBindIdListByService srv f s o) = getBindIdListByService srv f s o
fetchReq (CountBindByUIDAndService uid srv) = countBindByUIDAndService uid srv
fetchReq (GetBindIdListByUIDAndService uid srv f s o) = getBindIdListByUIDAndService uid srv f s o
fetchReq (RemoveBindByUID uid)            = removeBindByUID uid

fetchReq MergeData                        = mergeData

fetchReq (AddGroup n uid)                 = addGroup n uid
fetchReq (RemoveGroup n uid)              = removeGroup n uid
fetchReq (GetGroupListByUserId uid)       = getGroupListByUserId uid
fetchReq (GetUserIdListByGroup n f s o)   = getUserIdListByGroup n f s o
fetchReq (RemoveGroupListByUserId uid)    = removeGroupListByUserId uid
fetchReq (CountGroup n)                   = countGroup n
fetchReq (SaveGroupMeta n t s)            = saveGroupMeta n t s
fetchReq (GetGroupMeta n)                 = getGroupMeta n
fetchReq (RemoveGroupMeta n)              = removeGroupMeta n
fetchReq GetGroupMetaList                 = getGroupMetaList


initUserState :: Int -> State UserReq
initUserState = UserState
