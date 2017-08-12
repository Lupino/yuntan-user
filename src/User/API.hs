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
import           Haxl.Core               (dataFetch, uncachedRequest)

import           User.DataSource
import           User.Types
import           User.UserEnv            (UserM)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createUser         :: UserName -> Password -> UserM UserID
getUser            :: UserID -> UserM (Maybe User)
getUserByName      :: UserName -> UserM (Maybe User)
removeUser         :: UserID -> UserM Int64
updateUserName     :: UserID -> UserName -> UserM Int64
updateUserPassword :: UserID -> Password -> UserM Int64
updateUserExtra    :: UserID -> Extra -> UserM Int64
getUsers           :: From -> Size -> OrderBy -> UserM [User]
countUser          :: UserM Int64

createUser name passwd        = uncachedRequest (CreateUser name passwd)
getUser uid                   = fillBinds =<< dataFetch (GetUser uid)
getUserByName name            = fillBinds =<< dataFetch (GetUserByName name)
removeUser uid                = uncachedRequest (RemoveUser uid)
updateUserName uid name       = uncachedRequest (UpdateUserName uid name)
updateUserPassword uid passwd = uncachedRequest (UpdateUserPassword uid passwd)
updateUserExtra uid extra     = uncachedRequest (UpdateUserExtra uid extra)
getUsers from size order      = catMaybes <$> (mapM (\u -> fillBinds (Just u))
                                      =<< dataFetch (GetUsers from size order))
countUser                     = dataFetch CountUser

createBind         :: UserID -> Service -> ServiceName -> Extra -> UserM BindID
getBind            :: BindID -> UserM (Maybe Bind)
getBindByName      :: ServiceName -> UserM (Maybe Bind)
removeBind         :: BindID -> UserM Int64
updateBindExtra    :: BindID -> Extra -> UserM Int64
countBind          :: UserID -> UserM Int64
getBinds           :: UserID -> UserM [Bind]
removeBinds        :: UserID -> UserM Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = dataFetch (GetBind bid)
getBindByName n        = dataFetch (GetBindByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBind uid          = dataFetch (CountBind uid)
getBinds uid           = dataFetch (GetBinds uid)
removeBinds uid        = uncachedRequest (RemoveBinds uid)

fillBinds :: Maybe User -> UserM (Maybe User)
fillBinds (Just u@(User { getUserID = uid })) = do
  binds <- getBinds uid
  return (Just u { getUserBinds = binds })

fillBinds Nothing = return Nothing

createTable :: UserM Int64
createTable = uncachedRequest CreateTable
