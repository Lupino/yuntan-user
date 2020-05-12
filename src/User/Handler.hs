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
  , updateUserSecureExtraHandler
  , removeUserSecureExtraHandler
  , clearUserSecureExtraHandler
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

  , saveGroupMetaHandler
  , getGroupMetaHandler
  , getGroupMetaListHandler
  , removeGroupMetaHandler

  , graphqlHandler
  , graphqlByBindHandler
  , graphqlByUserHandler
  , graphqlByServiceHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl)
import           User
import           Web.Scotty.Trans        (json, param, rescue)
import           Yuntan.Types.HasPSQL    (HasOtherEnv, HasPSQL)
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

createUserHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
createUserHandler = do
  name <- param "username"
  passwd <- hashPassword <$> param "passwd"

  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do

      uid <- lift $ createUser (pack name) (pack passwd)
      json =<< lift (toOUser' <$> getUser uid)

verifyPasswordHandler :: HasPSQL u => User -> ActionH u w ()
verifyPasswordHandler User{getUserPassword = pwd} = do
  valid <- flip isVaildPassword (unpack pwd) <$> param "passwd"
  if valid then resultOK
           else errorInvalidPassword

errorInvalidPassword :: ActionH u w ()
errorInvalidPassword = errBadRequest "invalid password"

errorInvalidUserName :: ActionH u w ()
errorInvalidUserName = errBadRequest $ "invalid username, the valid username char is " ++ validUserName

errorInvalidUserName' :: ActionH u w ()
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

apiUser :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe User)
apiUser = do
  name <- param "uidOrName"
  user <- lift $ getUserByName (pack name)
  case user of
    Just u -> return $ Just u
    Nothing ->
      if isDigest name then lift (getUser $ read name)
                       else return Nothing

getUserHandler :: User -> ActionH u w ()
getUserHandler = json . toOUser

requireUser :: (HasPSQL u, HasOtherEnv Cache u) => (User -> ActionH u w ()) -> ActionH u w ()
requireUser act = do
  user <- apiUser
  case user of
    Just u  -> act u
    Nothing ->  errorUserNotFound

  where errorUserNotFound :: ActionH u w ()
        errorUserNotFound = errNotFound "User is not found."

removeUserHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
removeUserHandler User{getUserID = uid} = do
  void . lift $ removeUser uid
  void . lift $ removeBindByUID uid
  void . lift $ removeGroupListByUserId uid
  resultOK

updateUserNameHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
updateUserNameHandler User{getUserID = uid} = do
  name <- param "username"
  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do
      void . lift $ updateUserName uid (pack name)
      resultOK

updateUserPasswordHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
updateUserPasswordHandler User{getUserID = uid} = do
  passwd <- pack . hashPassword <$> param "passwd"
  void . lift $ updateUserPassword uid passwd
  resultOK

updateUserExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
updateUserExtraHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateUserExtra uid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

errorExtraRequired :: ActionH u w ()
errorExtraRequired = errBadRequest "extra field is required."

removeUserExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
removeUserExtraHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserExtra uid $ differenceValue oev ev) >> resultOK
    Nothing -> errorExtraRequired

clearUserExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
clearUserExtraHandler User{getUserID = uid} =
  void (lift $ updateUserExtra uid Null) >> resultOK

updateUserSecureExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
updateUserSecureExtraHandler User{getUserID = uid, getUserSecureExtra = oev} = do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateUserSecureExtra uid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

removeUserSecureExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
removeUserSecureExtraHandler User{getUserID = uid, getUserSecureExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserSecureExtra uid $ differenceValue oev ev) >> resultOK
    Nothing -> errorExtraRequired

clearUserSecureExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
clearUserSecureExtraHandler User{getUserID = uid} =
  void (lift $ updateUserSecureExtra uid Null) >> resultOK

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f c a b = f a b c

paramPage :: ActionH u w (From, Size)
paramPage = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  pure (from, size)


getUsersHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getUsersHandler = userListHandler countUser (flip' getUsers (desc "id"))

getUserListByGroupHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getUserListByGroupHandler = do
  group <- param "group"
  userListHandler (countGroup group) (flip' (getUserListByGroup group) (desc "user_id"))

userListHandler :: HasPSQL u => GenHaxl u w Int64 -> (From -> Size -> GenHaxl u w [User]) ->  ActionH u w ()
userListHandler count userList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ userList from size
  okListResult "users" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = map toOUser users
                                  }

apiBind :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Bind)
apiBind = do
  name <- param "bidOrName"
  user <- lift $ getBindByName (pack name)
  case user of
    Just u -> return $ Just u
    Nothing ->
      if isDigest name then lift (getBind $ read name)
                       else return Nothing

requireBind :: (HasPSQL u, HasOtherEnv Cache u) => (Bind -> ActionH u w ()) -> ActionH u w ()
requireBind act = do
  bind <- apiBind
  case bind of
    Just b  -> act b
    Nothing ->  errorBindNotFound

  where errorBindNotFound :: ActionH u w ()
        errorBindNotFound = errNotFound "Bind is not found."

createBindHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
createBindHandler User{getUserID = uid} = do
  service <- param "service"
  name <- param "name"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))
  bid <- lift $ createBind uid service name $ fromMaybe Null extra
  json =<< lift (getBind bid)

getBindHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getBindHandler = do
  name <- param "name"
  maybeNotFound "Bind" =<< lift (getBindByName name)

removeBindHandler :: (HasPSQL u, HasOtherEnv Cache u) => Bind -> ActionH u w ()
removeBindHandler Bind{getBindID=bid}= do
  void . lift $ removeBind bid
  resultOK

updateBindExtraHandler :: (HasPSQL u, HasOtherEnv Cache u) => Bind -> ActionH u w ()
updateBindExtraHandler Bind{getBindID=bid, getBindExtra=oev}= do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateBindExtra bid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

getBindListByServiceHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getBindListByServiceHandler = do
  srv <- param "service"
  bindListHandler (countBindByService srv) (flip' (getBindListByService srv) (desc "id"))

getBindListByUserHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
getBindListByUserHandler User{getUserID = uid}=
  bindListHandler (countBindByUID uid) (flip' (getBindListByUID uid) (desc "id"))

getBindListByUserAndServiceHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
getBindListByUserAndServiceHandler User{getUserID = uid}= do
  srv <- param "service"
  bindListHandler (countBindByUIDAndService uid srv) (flip' (getBindListByUIDAndService uid srv) (desc "id"))

bindListHandler :: HasPSQL u => GenHaxl u w Int64 -> (From -> Size -> GenHaxl u w [Bind]) -> ActionH u w ()
bindListHandler count bindList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ bindList from size
  okListResult "binds" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = users
                                  }

createGroupHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
createGroupHandler User{getUserID = uid}= do
  group <- param "group"
  lift $ void $ addGroup group uid
  resultOK

removeGroupHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
removeGroupHandler User{getUserID = uid} = do
  group <- param "group"
  void . lift $ removeGroup group uid
  resultOK

saveGroupMetaHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
saveGroupMetaHandler = do
  group <- param "group"
  title <- param "title"
  summary <- param "summary"
  void . lift $ saveGroupMeta group title summary
  resultOK

getGroupMetaHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getGroupMetaHandler = do
  group <- param "group"
  maybeNotFound "GroupMeta" =<< lift (getGroupMeta group)

removeGroupMetaHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
removeGroupMetaHandler = do
  group <- param "group"
  void . lift $ removeGroupMeta group
  resultOK

getGroupMetaListHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getGroupMetaListHandler = json =<< lift getGroupMetaList

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: String)

graphqlHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
graphqlHandler = do
  query <- param "query"
  json =<< lift (graphql schema query)

graphqlByBindHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
graphqlByBindHandler = do
  name <- param "name"
  b <- lift (getBindByName name)
  case b of
    Nothing -> errNotFound "Bind is not found"
    Just b' -> do
      query <- param "query"
      json =<< lift (graphql (schemaByBind b') query)

graphqlByUserHandler :: (HasPSQL u, HasOtherEnv Cache u) => User -> ActionH u w ()
graphqlByUserHandler u = do
  query <- param "query"
  json =<< lift (graphql (schemaByUser u) query)

graphqlByServiceHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
graphqlByServiceHandler = do
  query <- param "query"
  srv <- param "service"
  json =<< lift (graphql (schemaByService srv) query)
