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
  , countBind
  , getBinds
  , removeBinds

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
import           Yuntan.Types.HasMySQL   (HasMySQL)

import           User.DataSource
import           User.Types
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createUser         :: HasMySQL u => UserName -> Password -> GenHaxl u UserID
getUser            :: HasMySQL u => UserID -> GenHaxl u (Maybe User)
getUserByName      :: HasMySQL u => UserName -> GenHaxl u (Maybe User)
removeUser         :: HasMySQL u => UserID -> GenHaxl u Int64
updateUserName     :: HasMySQL u => UserID -> UserName -> GenHaxl u Int64
updateUserPassword :: HasMySQL u => UserID -> Password -> GenHaxl u Int64
updateUserExtra    :: HasMySQL u => UserID -> Extra -> GenHaxl u Int64
getUsers           :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [User]
countUser          :: HasMySQL u => GenHaxl u Int64

createUser name passwd        = uncachedRequest (CreateUser name passwd)
getUser uid                   = fillGroups =<< fillBinds =<< dataFetch (GetUser uid)
getUserByName name            = fillGroups =<< fillBinds =<< dataFetch (GetUserByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
getUsers from size order      = do
  users <- dataFetch (GetUsers from size order)
  catMaybes <$> (for users $ \user -> fillGroups =<< fillBinds (Just user))

countUser                     = dataFetch CountUser

createBind         :: HasMySQL u => UserID -> Service -> ServiceName -> Extra -> GenHaxl u BindID
getBind            :: HasMySQL u => BindID -> GenHaxl u (Maybe Bind)
getBindByName      :: HasMySQL u => ServiceName -> GenHaxl u (Maybe Bind)
removeBind         :: HasMySQL u => BindID -> GenHaxl u Int64
updateBindExtra    :: HasMySQL u => BindID -> Extra -> GenHaxl u Int64
countBind          :: HasMySQL u => UserID -> GenHaxl u Int64
getBinds           :: HasMySQL u => UserID -> GenHaxl u [Bind]
removeBinds        :: HasMySQL u => UserID -> GenHaxl u Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = dataFetch (GetBind bid)
getBindByName n        = dataFetch (GetBindByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBind uid          = dataFetch (CountBind uid)
getBinds uid           = dataFetch (GetBinds uid)
removeBinds uid        = uncachedRequest (RemoveBinds uid)

fillBinds :: HasMySQL u => Maybe User -> GenHaxl u (Maybe User)
fillBinds (Just u@User{getUserID = uid}) = do
  binds <- getBinds uid
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

getUserListByGroup :: HasMySQL u => GroupName -> From -> Size -> OrderBy -> GenHaxl u [User]
getUserListByGroup group f s o = do
  uids <- getUserIDListByGroup group f s o
  catMaybes <$> for uids getUser
