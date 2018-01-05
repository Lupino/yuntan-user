{-# LANGUAGE OverloadedStrings #-}
module User.APIHandler
  (
    createUserAPIHandler
  , requireUser
  , getUserAPIHandler
  , removeUserAPIHandler
  , updateUserNameAPIHandler
  , updateUserPasswordAPIHandler
  , updateUserExtraAPIHandler
  , removeUserExtraAPIHandler
  , clearUserExtraAPIHandler
  , getUsersAPIHandler
  , verifyPasswordAPIHandler

  , createBindAPIHandler
  , getBindAPIHandler
  , removeBindAPIHandler
  , getBindListByServiceAPIHandler
  , getBindListByUserAPIHandler
  , getBindListByUserAndServiceAPIHandler

  , createGroupAPIHandler
  , removeGroupAPIHandler
  , getUserListByGroupAPIHandler

  , graphqlHandler
  , graphqlByBindHandler
  , graphqlByUserHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl)
import           User
import           Web.Scotty.Trans        (json, param, rescue)
import           Yuntan.Types.HasMySQL   (HasMySQL)
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
import           User.GraphQL            (schema, schemaByBind, schemaByUser)

createUserAPIHandler :: HasMySQL u => ActionH u ()
createUserAPIHandler = do
  name <- param "username"
  passwd <- hashPassword <$> param "passwd"

  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do

      uid <- lift $ createUser (pack name) (pack passwd)
      json =<< lift (getUser uid)

verifyPasswordAPIHandler :: HasMySQL u => User -> ActionH u ()
verifyPasswordAPIHandler User{getUserPassword = pwd} = do
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

getUserAPIHandler :: User -> ActionH u ()
getUserAPIHandler = json

requireUser :: HasMySQL u => (User -> ActionH u ()) -> ActionH u ()
requireUser act = do
  user <- apiUser
  case user of
    Just u  -> act u
    Nothing ->  errorUserNotFound

  where errorUserNotFound :: ActionH u ()
        errorUserNotFound = errNotFound "User is not found."

removeUserAPIHandler :: HasMySQL u => User -> ActionH u ()
removeUserAPIHandler User{getUserID = uid} = do
  void . lift $ removeUser uid
  void . lift $ removeBindByUID uid
  void . lift $ removeGroupListByUserID uid
  resultOK

updateUserNameAPIHandler :: HasMySQL u => User -> ActionH u ()
updateUserNameAPIHandler User{getUserID = uid} = do
  name <- param "username"
  case (isDigest name, isValidUserName name) of
    (True, _) -> errorInvalidUserName'
    (False, False) -> errorInvalidUserName
    (False, True) -> do
      void . lift $ updateUserName uid (pack name)
      resultOK

updateUserPasswordAPIHandler :: HasMySQL u => User -> ActionH u ()
updateUserPasswordAPIHandler User{getUserID = uid} = do
  passwd <- pack . hashPassword <$> param "passwd"
  void . lift $ updateUserPassword uid passwd
  resultOK

updateUserExtraAPIHandler :: HasMySQL u => User -> ActionH u ()
updateUserExtraAPIHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateUserExtra uid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

errorExtraRequired :: ActionH u ()
errorExtraRequired = errBadRequest "extra field is required."

removeUserExtraAPIHandler :: HasMySQL u => User -> ActionH u ()
removeUserExtraAPIHandler User{getUserID = uid, getUserExtra = oev} = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserExtra uid $ differenceValue oev ev) >> resultOK
    Nothing -> errorExtraRequired

clearUserExtraAPIHandler :: HasMySQL u => User -> ActionH u ()
clearUserExtraAPIHandler User{getUserID = uid} =
  void (lift $ updateUserExtra uid Null) >> resultOK


flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f c a b = f a b c

paramPage :: ActionH u (From, Size)
paramPage = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  pure (from, size)


getUsersAPIHandler :: HasMySQL u => ActionH u ()
getUsersAPIHandler = userListAPIHandler countUser (flip' getUsers (desc "id"))

getUserListByGroupAPIHandler :: HasMySQL u => ActionH u ()
getUserListByGroupAPIHandler = do
  group <- param "group"
  userListAPIHandler (countGroup group) (flip' (getUserListByGroup group) (desc "user_id"))

userListAPIHandler :: HasMySQL u => GenHaxl u Int64 -> (From -> Size -> GenHaxl u [User]) ->  ActionH u ()
userListAPIHandler count userList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ userList from size
  okListResult "users" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = users
                                  }

createBindAPIHandler :: HasMySQL u => User -> ActionH u ()
createBindAPIHandler User{getUserID = uid} = do
  service <- param "service"
  name <- param "name"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))
  bid <- lift $ createBind uid service name $ fromMaybe Null extra
  json =<< lift (getBind bid)

getBindAPIHandler :: HasMySQL u => ActionH u ()
getBindAPIHandler = do
  name <- param "name"
  maybeNotFound "Bind" =<< lift (getBindByName name)

removeBindAPIHandler :: HasMySQL u => ActionH u ()
removeBindAPIHandler = do
  bid <- param "bind_id"
  void . lift $ removeBind bid
  resultOK

getBindListByServiceAPIHandler :: HasMySQL u => ActionH u ()
getBindListByServiceAPIHandler = do
  srv <- param "service"
  bindListAPIHandler (countBindByService srv) (flip' (getBindListByService srv) (desc "id"))

getBindListByUserAPIHandler :: HasMySQL u => User -> ActionH u ()
getBindListByUserAPIHandler User{getUserID = uid}=
  bindListAPIHandler (countBindByUID uid) (flip' (getBindListByUID uid) (desc "id"))

getBindListByUserAndServiceAPIHandler :: HasMySQL u => User -> ActionH u ()
getBindListByUserAndServiceAPIHandler User{getUserID = uid}= do
  srv <- param "service"
  bindListAPIHandler (countBindByUIDAndService uid srv) (flip' (getBindListByUIDAndService uid srv) (desc "id"))

bindListAPIHandler :: HasMySQL u => GenHaxl u Int64 -> (From -> Size -> GenHaxl u [Bind]) -> ActionH u ()
bindListAPIHandler count bindList = do
  (from, size) <- paramPage
  total <- lift count
  users <- lift $ bindList from size
  okListResult "binds" ListResult { getFrom   = from
                                  , getSize   = size
                                  , getTotal  = total
                                  , getResult = users
                                  }

createGroupAPIHandler :: HasMySQL u => User -> ActionH u ()
createGroupAPIHandler User{getUserID = uid}= do
  group <- param "group"
  lift $ addGroup group uid
  resultOK

removeGroupAPIHandler :: HasMySQL u => User -> ActionH u ()
removeGroupAPIHandler User{getUserID = uid} = do
  group <- param "group"
  void . lift $ removeGroup group uid
  resultOK

resultOK :: ActionH u ()
resultOK = ok "result" ("OK" :: String)

graphqlHandler :: HasMySQL u => ActionH u ()
graphqlHandler = do
  query <- param "query"
  json =<< lift (graphql schema query)

graphqlByBindHandler :: HasMySQL u => ActionH u ()
graphqlByBindHandler = do
  name <- param "name"
  b <- lift (getBindByName name)
  case b of
    Nothing -> errNotFound "Bind is not found"
    Just b' -> do
      query <- param "query"
      json =<< lift (graphql (schemaByBind b') query)

graphqlByUserHandler :: HasMySQL u => User -> ActionH u ()
graphqlByUserHandler u = do
  query <- param "query"
  json =<< lift (graphql (schemaByUser u) query)
