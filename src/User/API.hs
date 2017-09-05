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

  , createTable
  ) where

import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes)
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
getUser uid                   = fillBinds =<< dataFetch (GetUser uid)
getUserByName name            = fillBinds =<< dataFetch (GetUserByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
getUsers from size order      = catMaybes <$> (mapM (fillBinds . Just)
                                      =<< dataFetch (GetUsers from size order))
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

createTable :: HasMySQL u => GenHaxl u Int64
createTable = uncachedRequest CreateTable
