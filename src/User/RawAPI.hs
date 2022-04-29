{-# LANGUAGE FlexibleContexts #-}

module User.RawAPI
  ( createUser
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
  , getBindIdByName
  , removeBind
  , updateBindExtra
  , countBindByUID
  , getBindIdListByUID
  , countBindByService
  , getBindIdListByService
  , countBindByUIDAndService
  , getBindIdListByUIDAndService
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

import           Data.Int            (Int64)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)
import           User.DataSource
import           User.Types

createUser         :: HasPSQL u => UserName -> Password -> GenHaxl u w UserID
getUser :: HasPSQL u => UserID -> GenHaxl u w (Maybe User)
getUserIdByName :: HasPSQL u => UserName -> GenHaxl u w (Maybe UserID)
removeUser         :: HasPSQL u => UserID -> GenHaxl u w Int64
updateUserName     :: HasPSQL u => UserID -> UserName -> GenHaxl u w Int64
updateUserPassword :: HasPSQL u => UserID -> Password -> GenHaxl u w Int64
updateUserExtra    :: HasPSQL u => UserID -> Extra -> GenHaxl u w Int64
updateUserSecureExtra    :: HasPSQL u => UserID -> Extra -> GenHaxl u w Int64
getUserIdList :: HasPSQL u => From -> Size -> OrderBy -> GenHaxl u w [UserID]
countUser          :: HasPSQL u => GenHaxl u w Int64

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

createBind         :: HasPSQL u => UserID -> Service -> ServiceName -> Extra -> GenHaxl u w BindID
getBind            :: HasPSQL u => BindID -> GenHaxl u w (Maybe Bind)
getBindIdByName    :: HasPSQL u => ServiceName -> GenHaxl u w (Maybe BindID)
removeBind         :: HasPSQL u => BindID -> GenHaxl u w Int64
updateBindExtra    :: HasPSQL u => BindID -> Extra -> GenHaxl u w Int64
countBindByUID     :: HasPSQL u => UserID -> GenHaxl u w Int64
getBindIdListByUID :: HasPSQL u => UserID -> From -> Size -> OrderBy -> GenHaxl u w [BindID]
countBindByService :: HasPSQL u => Service -> GenHaxl u w Int64
getBindIdListByService
  :: HasPSQL u => Service -> From -> Size -> OrderBy -> GenHaxl u w [BindID]
countBindByUIDAndService :: HasPSQL u => UserID -> Service -> GenHaxl u w Int64
getBindIdListByUIDAndService
  :: HasPSQL u
  => UserID -> Service -> From -> Size -> OrderBy -> GenHaxl u w [BindID]
removeBindByUID    :: HasPSQL u => UserID -> GenHaxl u w Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = dataFetch (GetBind bid)
getBindIdByName n        = dataFetch (GetBindIdByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBindByUID uid     = dataFetch (CountBindByUID uid)
getBindIdListByUID uid f s o = dataFetch (GetBindIdListByUID uid f s o)
countBindByService srv     = dataFetch (CountBindByService srv)
getBindIdListByService srv f s o = dataFetch (GetBindIdListByService srv f s o)
countBindByUIDAndService uid srv = dataFetch (CountBindByUIDAndService uid srv)
getBindIdListByUIDAndService uid srv f s o = dataFetch (GetBindIdListByUIDAndService uid srv f s o)
removeBindByUID uid    = uncachedRequest (RemoveBindByUID uid)

mergeData :: HasPSQL u => GenHaxl u w ()
mergeData = uncachedRequest MergeData

addGroup                :: HasPSQL u => GroupName -> UserID -> GenHaxl u w Int64
removeGroup             :: HasPSQL u => GroupName -> UserID -> GenHaxl u w Int64
getGroupListByUserId    :: HasPSQL u => UserID -> GenHaxl u w [GroupName]
getUserIdListByGroup    :: HasPSQL u => GroupName -> From -> Size -> OrderBy -> GenHaxl u w [UserID]
removeGroupListByUserId :: HasPSQL u => UserID -> GenHaxl u w Int64
countGroup              :: HasPSQL u => GroupName -> GenHaxl u w Int64

addGroup n uid               = uncachedRequest (AddGroup n uid)
removeGroup n uid            = uncachedRequest (RemoveGroup n uid)
getGroupListByUserId uid     = dataFetch (GetGroupListByUserId uid)
getUserIdListByGroup n f s o = dataFetch (GetUserIdListByGroup n f s o)
removeGroupListByUserId uid  = uncachedRequest (RemoveGroupListByUserId uid)
countGroup n                 = dataFetch (CountGroup n)

saveGroupMeta    :: HasPSQL u => GroupName -> GroupTitle -> GroupSummary -> GenHaxl u w Int64
getGroupMeta     :: HasPSQL u => GroupName -> GenHaxl u w (Maybe GroupMeta)
removeGroupMeta  :: HasPSQL u => GroupName -> GenHaxl u w Int64
getGroupMetaList :: HasPSQL u => GenHaxl u w [GroupMeta]

saveGroupMeta n t s = uncachedRequest (SaveGroupMeta n t s)
getGroupMeta n      = dataFetch (GetGroupMeta n)
removeGroupMeta n   = uncachedRequest (RemoveGroupMeta n)
getGroupMetaList    = dataFetch GetGroupMetaList
