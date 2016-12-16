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
  ) where

import           Control.Monad             (void)
import           Control.Monad.Reader      (lift)

import           Dispatch.Types.ListResult (From, ListResult (..), Size,
                                            fromListResult)
import           Dispatch.Types.OrderBy    (desc)
import           Dispatch.Types.Result     (err, ok)
import           Dispatch.Utils.JSON       (differenceValue, unionValue)
import           Dispatch.Utils.Scotty     (maybeNotFound)
import           Network.HTTP.Types        (status400, status404)
import           User
import           Web.Scotty.Trans          (body, json, param, rescue, status)

import           Data.Aeson                (ToJSON, Value (..), decode)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (pack, unpack)

createUserAPIHandler :: ActionM ()
createUserAPIHandler = do
  name <- param "username"
  passwd <- hashPassword <$> param "passwd"

  case (isDigest name, isValidUserName name) of
    (True, _) -> status status400 >> errorInvalidUserName'
    (False, False) -> status status400 >> errorInvalidUserName
    (False, True) -> do

      uid <- lift $ createUser (pack name) (pack passwd)
      json =<< lift (getUser uid)

verifyPasswordAPIHandler :: User -> ActionM ()
verifyPasswordAPIHandler (User { getUserPassword = pwd }) = do
  valid <- flip isVaildPassword (unpack pwd) <$> param "passwd"
  if valid then resultOK
           else status status400 >> errorInvalidPassword

errorInvalidPassword :: ActionM ()
errorInvalidPassword = json $ err "invalid password"

errorInvalidUserName :: ActionM ()
errorInvalidUserName = json . err $ "invalid username, the valid username char is " ++ validUserName

errorInvalidUserName' :: ActionM ()
errorInvalidUserName' = json $ err "invalid username, the valid username need one or more char which is not a number."

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

apiUser :: ActionM (Maybe User)
apiUser = do
  name <- param "uidOrName"
  user <- lift $ getUserByName (pack name)
  case user of
    Just user -> return $ Just user
    Nothing -> do
      if isDigest name then lift (getUser $ read name)
                       else return Nothing

getUserAPIHandler :: User -> ActionM ()
getUserAPIHandler = json

requireUser :: (User -> ActionM()) -> ActionM ()
requireUser act = do
  user <- apiUser
  case user of
    Just u  -> act u
    Nothing ->  errorUserNotFound

  where errorUserNotFound :: ActionM ()
        errorUserNotFound = status status404 >> json (err "User is not found.")

removeUserAPIHandler :: User -> ActionM ()
removeUserAPIHandler (User { getUserID = uid }) = do
  lift $ removeUser uid
  lift $ removeBinds uid
  resultOK

updateUserNameAPIHandler :: User -> ActionM ()
updateUserNameAPIHandler (User { getUserID = uid }) = do
  name <- param "username"
  case (isDigest name, isValidUserName name) of
    (True, _) -> status status400 >> errorInvalidUserName'
    (False, False) -> status status400 >> errorInvalidUserName
    (False, True) -> do
      lift $ updateUserName uid (pack name)
      resultOK

updateUserPasswordAPIHandler :: User -> ActionM ()
updateUserPasswordAPIHandler (User { getUserID = uid }) = do
  passwd <- pack . hashPassword <$> param "passwd"
  lift $ updateUserPassword uid passwd
  resultOK

updateUserExtraAPIHandler :: User -> ActionM ()
updateUserExtraAPIHandler (User { getUserID = uid, getUserExtra = oev }) = do
  extra <- param "extra"
  case (decode extra :: Maybe Extra) of
    Just ev -> void (lift $ updateUserExtra uid $ unionValue ev oev) >> resultOK
    Nothing -> errorExtraRequired

errorExtraRequired :: ActionM ()
errorExtraRequired = status status400 >> json (err "extra field is required.")

removeUserExtraAPIHandler :: User -> ActionM ()
removeUserExtraAPIHandler (User { getUserID = uid, getUserExtra = oev }) = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserExtra uid $ differenceValue oev ev) >> resultOK
    Nothing -> status status400 >> errorExtraRequired

clearUserExtraAPIHandler :: User -> ActionM ()
clearUserExtraAPIHandler (User { getUserID = uid }) = do
  void (lift $ updateUserExtra uid Null) >> resultOK

getUsersAPIHandler :: ActionM ()
getUsersAPIHandler = do
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  total <- lift countUser
  users <- lift $ getUsers from size $ desc "id"

  json . fromListResult "users" $ ListResult { getFrom   = from
                                             , getSize   = size
                                             , getTotal  = total
                                             , getResult = users
                                             }

createBindAPIHandler :: User -> ActionM ()
createBindAPIHandler (User { getUserID = uid }) = do
  service <- param "service"
  name <- param "name"
  extra <- decode <$> (param "extra" `rescue` (\_ -> return "null"))
  bid <- lift $ createBind uid service name $ fromMaybe Null extra
  json =<< lift (getBind bid)

getBindAPIHandler :: ActionM ()
getBindAPIHandler = do
  name <- param "name"
  maybeNotFound "Bind" =<< lift (getBindByName name)

removeBindAPIHandler :: ActionM ()
removeBindAPIHandler = do
  bid <- param "bind_id"
  lift $ removeBind bid
  resultOK

resultOK :: ActionM ()
resultOK = json $ ok "OK"
