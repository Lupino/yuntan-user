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
  , updateUserSecureExtra
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

  , saveGroupMeta
  , getGroupMeta
  , removeGroupMeta
  , getGroupMetaList
  ) where

import           Data.Aeson              (Value)
import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes)
import           Data.Traversable        (for)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           User.Config             (Cache, lruEnv)
import           User.DataSource
import           User.Types
import           Yuntan.Extra.Config     (fillValue, fillValue_)
import           Yuntan.Types.HasMySQL   (HasMySQL, HasOtherEnv)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy, desc)

createUser         :: HasMySQL u => UserName -> Password -> GenHaxl u UserID
getUser
  :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u (Maybe User)
getUserByName
  :: (HasMySQL u, HasOtherEnv Cache u) => UserName -> GenHaxl u (Maybe User)
removeUser         :: HasMySQL u => UserID -> GenHaxl u Int64
updateUserName     :: HasMySQL u => UserID -> UserName -> GenHaxl u Int64
updateUserPassword :: HasMySQL u => UserID -> Password -> GenHaxl u Int64
updateUserExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
updateUserSecureExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
getUsers
  :: (HasMySQL u, HasOtherEnv Cache u)
  => From -> Size -> OrderBy -> GenHaxl u [User]
countUser          :: HasMySQL u => GenHaxl u Int64

createUser name passwd        = uncachedRequest (CreateUser name passwd)
getUser uid                   = fillUser =<< dataFetch (GetUser uid)
getUserByName name            = fillUser =<< dataFetch (GetUserByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
updateUserSecureExtra uid extra = uncachedRequest (UpdateUserSecureExtra uid extra)
getUsers from size order      = do
  users <- dataFetch (GetUsers from size order)
  catMaybes <$> for users (\user -> fillUser (Just user))

countUser                     = dataFetch CountUser

createBind         :: HasMySQL u => UserID -> Service -> ServiceName -> Extra -> GenHaxl u BindID
getBind
  :: (HasMySQL u, HasOtherEnv Cache u)
  => BindID -> GenHaxl u (Maybe Bind)
getBindByName
  :: (HasMySQL u, HasOtherEnv Cache u)
  => ServiceName -> GenHaxl u (Maybe Bind)
removeBind         :: HasMySQL u => BindID -> GenHaxl u Int64
updateBindExtra    :: HasMySQL u => BindID -> Extra -> GenHaxl u Int64
countBindByUID     :: HasMySQL u => UserID -> GenHaxl u Int64
getBindListByUID
  :: (HasMySQL u, HasOtherEnv Cache u)
  => UserID -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByService :: HasMySQL u => Service -> GenHaxl u Int64
getBindListByService
  :: (HasMySQL u, HasOtherEnv Cache u)
  => Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByUIDAndService :: HasMySQL u => UserID -> Service -> GenHaxl u Int64
getBindListByUIDAndService
  :: (HasMySQL u, HasOtherEnv Cache u)
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

fillBinds :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
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
  :: (HasMySQL u, HasOtherEnv Cache u)
  => GroupName -> From -> Size -> OrderBy -> GenHaxl u [User]
getUserListByGroup group f s o = do
  uids <- getUserIDListByGroup group f s o
  catMaybes <$> for uids getUser

fillUserExtra :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillUserExtra = fillValue lruEnv "user-extra" getUserExtra update
  where update :: Value -> User -> User
        update v u = u {getUserExtra = v}

fillUserSecureExtra :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillUserSecureExtra = fillValue lruEnv "user-secure-extra" getUserSecureExtra update
  where update :: Value -> User -> User
        update v u = u {getUserSecureExtra = v}

fillBindExtra :: (HasMySQL u, HasOtherEnv Cache u) => Maybe Bind -> GenHaxl u (Maybe Bind)
fillBindExtra = fillValue lruEnv "bind-extra" getBindExtra update
  where update :: Value -> Bind -> Bind
        update v u = u {getBindExtra = v}

fillBindExtra_ :: (HasMySQL u, HasOtherEnv Cache u) => Bind -> GenHaxl u Bind
fillBindExtra_ = fillValue_ lruEnv "bind-extra" getBindExtra update
  where update :: Value -> Bind -> Bind
        update v u = u {getBindExtra = v}

fillAllBindExtra_ :: (HasMySQL u, HasOtherEnv Cache u) => [Bind] -> GenHaxl u [Bind]
fillAllBindExtra_ = mapM fillBindExtra_

fillUser :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillUser u = fillUserSecureExtra =<< fillUserExtra =<< fillGroups =<< fillBinds u

saveGroupMeta    :: HasMySQL u => GroupName -> GroupTitle -> GroupSummary -> GenHaxl u Int64
getGroupMeta     :: HasMySQL u => GroupName -> GenHaxl u (Maybe GroupMeta)
removeGroupMeta  :: HasMySQL u => GroupName -> GenHaxl u Int64
getGroupMetaList :: HasMySQL u => GenHaxl u [GroupMeta]

saveGroupMeta n t s = uncachedRequest (SaveGroupMeta n t s)
getGroupMeta n      = fillGroupUserCount =<< dataFetch (GetGroupMeta n)
removeGroupMeta n   = uncachedRequest (RemoveGroupMeta n)
getGroupMetaList    = do
  gs <- dataFetch GetGroupMetaList
  catMaybes <$> for gs (\g -> fillGroupUserCount (Just g))

fillGroupUserCount :: HasMySQL u => Maybe GroupMeta -> GenHaxl u (Maybe GroupMeta)
fillGroupUserCount (Just g@GroupMeta{getGroup = group}) = do
  uc <- countGroup group
  return (Just g {getGroupUserCount = uc})

fillGroupUserCount Nothing = return Nothing
