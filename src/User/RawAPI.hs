{-# LANGUAGE FlexibleContexts #-}

module User.RawAPI
  (
    createUser
  , getUser
  , getUserIdByName
  , removeUser
  , updateUserName
  , updateUserPassword
  , updateUserExtra
  , updateUserSecureExtra
  , countUser
  , getUserIdList

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
  , removeGroupListByUserId
  , getUserIdListByGroup
  , getGroupListByUserId
  , countGroup

  , saveGroupMeta
  , getGroupMeta
  , removeGroupMeta
  , getGroupMetaList
  ) where

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           User.DataSource
import           User.Types
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createUser         :: HasMySQL u => UserName -> Password -> GenHaxl u UserID
getUser :: HasMySQL u => UserID -> GenHaxl u (Maybe User)
getUserIdByName :: HasMySQL u => UserName -> GenHaxl u (Maybe UserID)
removeUser         :: HasMySQL u => UserID -> GenHaxl u Int64
updateUserName     :: HasMySQL u => UserID -> UserName -> GenHaxl u Int64
updateUserPassword :: HasMySQL u => UserID -> Password -> GenHaxl u Int64
updateUserExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
updateUserSecureExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
getUserIdList :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [UserID]
countUser          :: HasMySQL u => GenHaxl u Int64

createUser name passwd        = uncachedRequest (CreateUser name passwd)
getUser uid                   = dataFetch (GetUser uid)
getUserIdByName name          = dataFetch (GetUserIdByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
updateUserSecureExtra uid ext = uncachedRequest (UpdateUserSecureExtra uid ext)
getUserIdList from size order = dataFetch (GetUserIdList from size order)
countUser                     = dataFetch CountUser

createBind         :: HasMySQL u => UserID -> Service -> ServiceName -> Extra -> GenHaxl u BindID
getBind :: HasMySQL u => BindID -> GenHaxl u (Maybe Bind)
getBindByName :: HasMySQL u => ServiceName -> GenHaxl u (Maybe Bind)
removeBind         :: HasMySQL u => BindID -> GenHaxl u Int64
updateBindExtra    :: HasMySQL u => BindID -> Extra -> GenHaxl u Int64
countBindByUID     :: HasMySQL u => UserID -> GenHaxl u Int64
getBindListByUID :: HasMySQL u => UserID -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByService :: HasMySQL u => Service -> GenHaxl u Int64
getBindListByService
  :: HasMySQL u => Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
countBindByUIDAndService :: HasMySQL u => UserID -> Service -> GenHaxl u Int64
getBindListByUIDAndService
  :: HasMySQL u
  => UserID -> Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
removeBindByUID    :: HasMySQL u => UserID -> GenHaxl u Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = dataFetch (GetBind bid)
getBindByName n        = dataFetch (GetBindByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBindByUID uid     = dataFetch (CountBindByUID uid)
getBindListByUID uid f s o = dataFetch (GetBindListByUID uid f s o)
countBindByService srv     = dataFetch (CountBindByService srv)
getBindListByService srv f s o = dataFetch (GetBindListByService srv f s o)
countBindByUIDAndService uid srv = dataFetch (CountBindByUIDAndService uid srv)
getBindListByUIDAndService uid srv f s o = dataFetch (GetBindListByUIDAndService uid srv f s o)
removeBindByUID uid    = uncachedRequest (RemoveBindByUID uid)

mergeData :: HasMySQL u => GenHaxl u ()
mergeData = uncachedRequest MergeData

addGroup                :: HasMySQL u => GroupName -> UserID -> GenHaxl u ()
removeGroup             :: HasMySQL u => String -> UserID -> GenHaxl u Int64
getGroupListByUserId    :: HasMySQL u => UserID -> GenHaxl u [GroupName]
getUserIdListByGroup    :: HasMySQL u => GroupName -> From -> Size -> OrderBy -> GenHaxl u [UserID]
removeGroupListByUserId :: HasMySQL u => UserID -> GenHaxl u Int64
countGroup              :: HasMySQL u => GroupName -> GenHaxl u Int64

addGroup n uid               = uncachedRequest (AddGroup n uid)
removeGroup n uid            = uncachedRequest (RemoveGroup n uid)
getGroupListByUserId uid     = dataFetch (GetGroupListByUserId uid)
getUserIdListByGroup n f s o = dataFetch (GetUserIdListByGroup n f s o)
removeGroupListByUserId uid  = uncachedRequest (RemoveGroupListByUserId uid)
countGroup n                 = dataFetch (CountGroup n)

saveGroupMeta    :: HasMySQL u => GroupName -> GroupTitle -> GroupSummary -> GenHaxl u Int64
getGroupMeta     :: HasMySQL u => GroupName -> GenHaxl u (Maybe GroupMeta)
removeGroupMeta  :: HasMySQL u => GroupName -> GenHaxl u Int64
getGroupMetaList :: HasMySQL u => GenHaxl u [GroupMeta]

saveGroupMeta n t s = uncachedRequest (SaveGroupMeta n t s)
getGroupMeta n      = dataFetch (GetGroupMeta n)
removeGroupMeta n   = uncachedRequest (RemoveGroupMeta n)
getGroupMetaList    = dataFetch GetGroupMetaList
