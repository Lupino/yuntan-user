{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module User.Handler
  (
    createUserHandler
  , requireUser
  , getUserHandler
  , removeUserHandler
  , updateUserNameHandler
  , updateUserPasswordHandler
  , updateUserExtraHandler
  , removeUserExtraHandler
  , clearUserExtraHandler
  , getUsersHandler
  , verifyPasswordHandler

  , createBindHandler
  , getBindHandler
  , removeBindHandler
  , requireBind
  , updateBindExtraHandler
  , getBindListByServiceHandler
  , getBindListByUserHandler
  , getBindListByUserAndServiceHandler

  , createGroupHandler
  , removeGroupHandler
  , getUserListByGroupHandler

  , graphqlHandler
  , graphqlByBindHandler
  , graphqlByUserHandler
  , graphqlByServiceHandler

  , setConfigHandler
  , getConfigHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)

import           Data.ByteString         (ByteString)
import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl)
import           User
import           Web.Scotty.Trans        (json, param, rescue)
import           Yuntan.Types.HasMySQL   (ConfigLru, HasMySQL, HasOtherEnv,
                                          getConfigJSON', otherEnv, setConfig')
import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.JSON       (differenceValue, unionValue)
import           Yuntan.Utils.Scotty     (errBadRequest, errNotFound,
                                          maybeNotFound, ok, okListResult)

import           Data.Aeson              (Value (..), decode)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack, unpack)

import           Data.GraphQL            (graphql)
import           User.GraphQL

createUserHandler :: HasMySQL u => ActionH u ()
createUserHandler = do
  name <- param "username"
  passwd <- hashPassword <$> param "passwd"

  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do

      uid <- lift $ createUser (pack name) (pack passwd)
      json =<< lift (getUser uid)

verifyPasswordHandler :: HasMySQL u => User -> ActionH u ()
verifyPasswordHandler User{getUserPassword = pwd} = do
  valid <- flip isVaildPassword (unpack pwd) <$> param "passwd"
  if valid then resultOK
           else errorInvalidPassword

errorInvalidPassword :: ActionH u ()
errorInvalidPassword = errBadRequest "invalid password"

errorInvalidUserName :: ActionH u ()
errorInvalidUserName = errBadRequest $ "invalid username, the valid username char is " ++ validUserName

errorInvalidUserName' :: ActionH u ()
errorInvalidUserName' = errBadRequest "invalid username, the valid username need one or more char which is not a number."

validUserName :: String
validUserName = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '_', '-']

isValidUserName :: String -> Bool
isValidUserName (x:xs) | x `elem` validUserName = isValidUserName xs
                       | otherwise              = False

isValidUserName [] = True

isDigest :: String -> Bool
isDigest (x:xs) | x `elem` ['0'..'9'] = isDigest xs
                | otherwise           = False

isDigest [] = True

apiUser :: HasMySQL u => ActionH u (Maybe User)
apiUser = do
  name <- param "uidOrName"
  user <- lift $ getUserByName (pack name)
  case user of
    Just u -> return $ Just u
    Nothing ->
      if isDigest name then lift (getUser $ read name)
                       else return Nothing

getUserHandler :: User -> ActionH u ()
getUserHandler = json

requireUser :: HasMySQL u => (User -> ActionH u ()) -> ActionH u ()
requireUser act = do
  user <- apiUser
  case user of
    Just u  -> act u
    Nothing ->  errorUserNotFound

  where errorUserNotFound :: ActionH u ()
        errorUserNotFound = errNotFound "User is not found."

removeUserHandler :: HasMySQL u => User -> ActionH u ()
removeUserHandler User{getUserID = uid} = do
  void . lift $ removeUser uid
  void . lift $ removeBindByUID uid
  void . lift $ removeGroupListByUserID uid
  resultOK

updateUserNameHandler :: HasMySQL u => User -> ActionH u ()
updateUserNameHandler User{getUserID = uid} = do
  name <- param "username"
  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do
      void . lift $ updateUserName uid (pack name)
      resultOK

updateUserPasswordHandler :: HasMySQL u => User -> ActionH u ()
updateUserPasswordHandler User{getUserID = uid} = do
  passwd <- pack . hashPassword <$> param "passwd"
  void . lift $ updateUserPassword uid passwd
  resultOK

updateUserExtraHandler :: HasMySQL u => User -> ActionH u ()
updateUserExtraHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateUserExtra uid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

errorExtraRequired :: ActionH u ()
errorExtraRequired = errBadRequest "extra field is required."

removeUserExtraHandler :: HasMySQL u => User -> ActionH u ()
removeUserExtraHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserExtra uid $ differenceValue oev ev) >> resultOK
    Nothing -> errorExtraRequired

clearUserExtraHandler :: HasMySQL u => User -> ActionH u ()
clearUserExtraHandler User{getUserID = uid} =
  void (lift $ updateUserExtra uid Null) >> resultOK


flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f c a b = f a b c

paramPage :: ActionH u (From, Size)
paramPage = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  pure (from, size)


getUsersHandler :: HasMySQL u => ActionH u ()
getUsersHandler = userListHandler countUser (flip' getUsers (desc "id"))

getUserListByGroupHandler :: HasMySQL u => ActionH u ()
getUserListByGroupHandler = do
  group <- param "group"
  userListHandler (countGroup group) (flip' (getUserListByGroup group) (desc "user_id"))

userListHandler :: HasMySQL u => GenHaxl u Int64 -> (From -> Size -> GenHaxl u [User]) ->  ActionH u ()
userListHandler count userList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ userList from size
  okListResult "users" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = users
                                  }

apiBind :: HasMySQL u => ActionH u (Maybe Bind)
apiBind = do
  name <- param "bidOrName"
  user <- lift $ getBindByName (pack name)
  case user of
    Just u -> return $ Just u
    Nothing ->
      if isDigest name then lift (getBind $ read name)
                       else return Nothing

requireBind :: HasMySQL u => (Bind -> ActionH u ()) -> ActionH u ()
requireBind act = do
  bind <- apiBind
  case bind of
    Just b  -> act b
    Nothing ->  errorBindNotFound

  where errorBindNotFound :: ActionH u ()
        errorBindNotFound = errNotFound "Bind is not found."

createBindHandler :: HasMySQL u => User -> ActionH u ()
createBindHandler User{getUserID = uid} = do
  service <- param "service"
  name <- param "name"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))
  bid <- lift $ createBind uid service name $ fromMaybe Null extra
  json =<< lift (getBind bid)

getBindHandler :: HasMySQL u => ActionH u ()
getBindHandler = do
  name <- param "name"
  maybeNotFound "Bind" =<< lift (getBindByName name)

removeBindHandler :: HasMySQL u => Bind -> ActionH u ()
removeBindHandler Bind{getBindID=bid}= do
  void . lift $ removeBind bid
  resultOK

updateBindExtraHandler :: HasMySQL u => Bind -> ActionH u ()
updateBindExtraHandler Bind{getBindID=bid, getBindExtra=oev}= do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateBindExtra bid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

getBindListByServiceHandler :: HasMySQL u => ActionH u ()
getBindListByServiceHandler = do
  srv <- param "service"
  bindListHandler (countBindByService srv) (flip' (getBindListByService srv) (desc "id"))

getBindListByUserHandler :: HasMySQL u => User -> ActionH u ()
getBindListByUserHandler User{getUserID = uid}=
  bindListHandler (countBindByUID uid) (flip' (getBindListByUID uid) (desc "id"))

getBindListByUserAndServiceHandler :: HasMySQL u => User -> ActionH u ()
getBindListByUserAndServiceHandler User{getUserID = uid}= do
  srv <- param "service"
  bindListHandler (countBindByUIDAndService uid srv) (flip' (getBindListByUIDAndService uid srv) (desc "id"))

bindListHandler :: HasMySQL u => GenHaxl u Int64 -> (From -> Size -> GenHaxl u [Bind]) -> ActionH u ()
bindListHandler count bindList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ bindList from size
  okListResult "binds" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = users
                                  }

createGroupHandler :: HasMySQL u => User -> ActionH u ()
createGroupHandler User{getUserID = uid}= do
  group <- param "group"
  lift $ addGroup group uid
  resultOK

removeGroupHandler :: HasMySQL u => User -> ActionH u ()
removeGroupHandler User{getUserID = uid} = do
  group <- param "group"
  void . lift $ removeGroup group uid
  resultOK

resultOK :: ActionH u ()
resultOK = ok "result" ("OK" :: String)

graphqlHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => ActionH u ()
graphqlHandler = do
  query <- param "query"
  json =<< lift (graphql schema query)

graphqlByBindHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => ActionH u ()
graphqlByBindHandler = do
  name <- param "name"
  b <- lift (getBindByName name)
  case b of
    Nothing -> errNotFound "Bind is not found"
    Just b' -> do
      query <- param "query"
      json =<< lift (graphql (schemaByBind b') query)

graphqlByUserHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => User -> ActionH u ()
graphqlByUserHandler u = do
  query <- param "query"
  json =<< lift (graphql (schemaByUser u) query)

graphqlByServiceHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => ActionH u ()
graphqlByServiceHandler = do
  query <- param "query"
  srv <- param "service"
  json =<< lift (graphql (schemaByService srv) query)

setConfigHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => ActionH u ()
setConfigHandler = do
  k <- param "key"
  v <- param "value"
  lift $ setConfig' otherEnv k v
  resultOK

getConfigHandler :: (HasMySQL u, HasOtherEnv ConfigLru u) => ActionH u ()
getConfigHandler = do
  k <- param "key"
  v <- lift (getConfigJSON' otherEnv k)
  json (v :: Maybe Value)
