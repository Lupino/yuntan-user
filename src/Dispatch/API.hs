module Dispatch.API
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

import           Data.Int            (Int64)
import           Data.Maybe          (catMaybes)
import           Haxl.Core           (dataFetch, uncachedRequest)

import           Dispatch.DataSource
import           Dispatch.Types
import           Dispatch.UserEnv    (DispatchM)

createUser         :: UserName -> Password -> DispatchM UserID
getUser            :: UserID -> DispatchM (Maybe User)
getUserByName      :: UserName -> DispatchM (Maybe User)
removeUser         :: UserID -> DispatchM Int64
updateUserName     :: UserID -> UserName -> DispatchM Int64
updateUserPassword :: UserID -> Password -> DispatchM Int64
updateUserExtra    :: UserID -> Extra -> DispatchM Int64
getUsers           :: From -> Size -> OrderBy -> DispatchM [User]
countUser          :: DispatchM Int64

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

createBind         :: UserID -> Service -> ServiceName -> Extra -> DispatchM BindID
getBind            :: BindID -> DispatchM (Maybe Bind)
getBindByName      :: ServiceName -> DispatchM (Maybe Bind)
removeBind         :: BindID -> DispatchM Int64
updateBindExtra    :: BindID -> Extra -> DispatchM Int64
countBind          :: UserID -> DispatchM Int64
getBinds           :: UserID -> DispatchM [Bind]
removeBinds        :: UserID -> DispatchM Int64

createBind uid se n ex = uncachedRequest (CreateBind uid se n ex)
getBind bid            = dataFetch (GetBind bid)
getBindByName n        = dataFetch (GetBindByName n)
removeBind bid         = uncachedRequest (RemoveBind bid)
updateBindExtra bid ex = uncachedRequest (UpdateBindExtra bid ex)
countBind uid          = dataFetch (CountBind uid)
getBinds uid           = dataFetch (GetBinds uid)
removeBinds uid        = uncachedRequest (RemoveBinds uid)

fillBinds :: Maybe User -> DispatchM (Maybe User)
fillBinds (Just u@(User { getUserID = uid })) = do
  binds <- getBinds uid
  return (Just u { getUserBinds = binds })

fillBinds Nothing = return Nothing

createTable :: DispatchM Int64
createTable = uncachedRequest CreateTable
