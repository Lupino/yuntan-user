{-# LANGUAGE FlexibleContexts #-}

module User.API
  (
    createUser
  , getUser
  , getUserByName
  , removeUser
  , updateUserName
  , updateUserPassword
  , updateUserExtra
  , countUser
  , getUsers

  , createBind
  , getBind
  , getBindByName
  , removeBind
  , updateBindExtra
  , countBindByUID
  , getBindListByUID
  , countBindByService
  , getBindListByService
  , countBindByUIDAndService
  , getBindListByUIDAndService
  , removeBindByUID

  , mergeData

  , addGroup
  , removeGroup
  , removeGroupListByUserID
  , getUserIDListByGroup
  , getGroupListByUserID
  , getUserListByGroup
  , countGroup
  ) where

import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes)
import           Data.Traversable        (for)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           Yuntan.Types.HasMySQL   (ConfigLru, HasMySQL, HasOtherEnv,
                                          fillValue, fillValue_, otherEnv)

import           Data.Aeson              (Value)
import           User.DataSource
import           User.Types
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy, desc)

createUser         :: HasMySQL u => UserName -> Password -> GenHaxl u UserID
getUser
  :: (HasMySQL u, HasOtherEnv ConfigLru u) => UserID -> GenHaxl u (Maybe User)
getUserByName
  :: (HasMySQL u, HasOtherEnv ConfigLru u) => UserName -> GenHaxl u (Maybe User)
removeUser         :: HasMySQL u => UserID -> GenHaxl u Int64
updateUserName     :: HasMySQL u => UserID -> UserName -> GenHaxl u Int64
updateUserPassword :: HasMySQL u => UserID -> Password -> GenHaxl u Int64
updateUserExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
getUsers
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => From -> Size -> OrderBy -> GenHaxl u [User]
countUser          :: HasMySQL u => GenHaxl u Int64

createUser name passwd        = uncachedRequest (CreateUser name passwd)
getUser uid                   = fillUserExtra =<< fillGroups =<< fillBinds =<< dataFetch (GetUser uid)
getUserByName name            = fillUserExtra =<< fillGroups =<< fillBinds =<< dataFetch (GetUserByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
getUsers from size order      = do
  users <- dataFetch (GetUsers from size order)
  catMaybes <$> for users (\user -> fillUserExtra =<< fillGroups =<< fillBinds (Just user))

countUser                     = dataFetch CountUser

createBind         :: HasMySQL u => UserID -> Service -> ServiceName -> Extra -> GenHaxl u BindID
getBind
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => BindID -> GenHaxl u (Maybe Bind)
getBindByName
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => ServiceName -> GenHaxl u (Maybe Bind)
removeBind         :: HasMySQL u => BindID -> GenHaxl u Int64
updateBindExtra    :: HasMySQL u => BindID -> Extra -> GenHaxl u Int64
countBindByUID     :: HasMySQL u => UserID -> GenHaxl u Int64
getBindListByUID
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => UserID -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByService :: HasMySQL u => Service -> GenHaxl u Int64
getBindListByService
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByUIDAndService :: HasMySQL u => UserID -> Service -> GenHaxl u Int64
getBindListByUIDAndService
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => UserID -> Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
removeBindByUID    :: HasMySQL u => UserID -> GenHaxl u Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = fillBindExtra =<< dataFetch (GetBind bid)
getBindByName n        = fillBindExtra =<< dataFetch (GetBindByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBindByUID uid     = dataFetch (CountBindByUID uid)
getBindListByUID uid f s o = fillAllBindExtra_ =<< dataFetch (GetBindListByUID uid f s o)
countBindByService srv     = dataFetch (CountBindByService srv)
getBindListByService srv f s o = fillAllBindExtra_ =<< dataFetch (GetBindListByService srv f s o)
countBindByUIDAndService uid srv = dataFetch (CountBindByUIDAndService uid srv)
getBindListByUIDAndService uid srv f s o = fillAllBindExtra_ =<< dataFetch (GetBindListByUIDAndService uid srv f s o)
removeBindByUID uid    = uncachedRequest (RemoveBindByUID uid)

fillBinds :: (HasMySQL u, HasOtherEnv ConfigLru u) => Maybe User -> GenHaxl u (Maybe User)
fillBinds (Just u@User{getUserID = uid}) = do
  binds <- getBindListByUID uid 0 5 $ desc "id"
  return (Just u { getUserBinds = binds })

fillBinds Nothing = return Nothing

mergeData :: HasMySQL u => GenHaxl u ()
mergeData = uncachedRequest MergeData

addGroup                :: HasMySQL u => GroupName -> UserID -> GenHaxl u ()
removeGroup             :: HasMySQL u => String -> UserID -> GenHaxl u Int64
getGroupListByUserID    :: HasMySQL u => UserID -> GenHaxl u [GroupName]
getUserIDListByGroup    :: HasMySQL u => GroupName -> From -> Size -> OrderBy -> GenHaxl u [UserID]
removeGroupListByUserID :: HasMySQL u => UserID -> GenHaxl u Int64
countGroup              :: HasMySQL u => GroupName -> GenHaxl u Int64

addGroup n uid               = uncachedRequest (AddGroup n uid)
removeGroup n uid            = uncachedRequest (RemoveGroup n uid)
getGroupListByUserID uid     = dataFetch (GetGroupListByUserID uid)
getUserIDListByGroup n f s o = dataFetch (GetUserIDListByGroup n f s o)
removeGroupListByUserID uid  = uncachedRequest (RemoveGroupListByUserID uid)
countGroup n                 = dataFetch (CountGroup n)

fillGroups :: HasMySQL u => Maybe User -> GenHaxl u (Maybe User)
fillGroups (Just u@User{getUserID = uid}) = do
  groups <- getGroupListByUserID uid
  return (Just u { getUserGroups = groups })

fillGroups Nothing = return Nothing

getUserListByGroup
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => GroupName -> From -> Size -> OrderBy -> GenHaxl u [User]
getUserListByGroup group f s o = do
  uids <- getUserIDListByGroup group f s o
  catMaybes <$> for uids getUser

fillUserExtra :: (HasMySQL u, HasOtherEnv ConfigLru u) => Maybe User -> GenHaxl u (Maybe User)
fillUserExtra = fillValue otherEnv "user-extra" getUserExtra update
  where update :: Value -> User -> User
        update v u = u {getUserExtra = v}

fillBindExtra :: (HasMySQL u, HasOtherEnv ConfigLru u) => Maybe Bind -> GenHaxl u (Maybe Bind)
fillBindExtra = fillValue otherEnv "bind-extra" getBindExtra update
  where update :: Value -> Bind -> Bind
        update v u = u {getBindExtra = v}

fillBindExtra_ :: (HasMySQL u, HasOtherEnv ConfigLru u) => Bind -> GenHaxl u Bind
fillBindExtra_ = fillValue_ otherEnv "bind-extra" getBindExtra update
  where update :: Value -> Bind -> Bind
        update v u = u {getBindExtra = v}

fillAllBindExtra_ :: (HasMySQL u, HasOtherEnv ConfigLru u) => [Bind] -> GenHaxl u [Bind]
fillAllBindExtra_ = mapM fillBindExtra_
