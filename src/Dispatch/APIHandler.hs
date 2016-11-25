{-# LANGUAGE OverloadedStrings #-}
module Dispatch.APIHandler
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

import           Control.Monad        (void)
import           Control.Monad.Reader (lift)

import           Dispatch
import           Network.HTTP.Types   (status400, status404)
import           Web.Scotty.Trans     (body, json, param, rescue, status)

import           Data.Aeson           (Value (..), decode, object, (.=))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (pack, unpack)

createUserAPIHandler :: ActionM ()
createUserAPIHandler = do
  name <- param "username"
  passwd <- hashPassword <$> param "passwd"

  uid <- lift $ createUser name (pack passwd)

  json =<< lift (getUser uid)

verifyPasswordAPIHandler :: User -> ActionM ()
verifyPasswordAPIHandler (User { getUserPassword = pwd }) = do
  valid <- flip isVaildPassword (unpack pwd) <$> param "passwd"
  if valid then resultOK
           else status status400 >> errorInvalidPassword

errorInvalidPassword :: ActionM ()
errorInvalidPassword = json $ object [ "err" .= pack "invalid password" ]

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

getUserAPIHandler :: ActionM ()
getUserAPIHandler = do
  json =<< apiUser

requireUser :: (User -> ActionM()) -> ActionM ()
requireUser act = do
  user <- apiUser
  case user of
    Just u  -> act u
    Nothing -> status status404 >> errorUserNotExists

  where errorUserNotExists :: ActionM ()
        errorUserNotExists = json $ object [ "err" .= pack "User is not exists." ]

removeUserAPIHandler :: User -> ActionM ()
removeUserAPIHandler (User { getUserID = uid }) = do
  lift $ removeUser uid
  lift $ removeBinds uid
  resultOK

updateUserNameAPIHandler :: User -> ActionM ()
updateUserNameAPIHandler (User { getUserID = uid }) = do
  name <- param "username"
  lift $ updateUserName uid name
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
    Just ev -> void (lift $ updateUserExtra uid $ unionExtra ev oev) >> resultOK
    Nothing -> status status400 >> errorExtraRequired

errorExtraRequired :: ActionM ()
errorExtraRequired = json $ object [ "err" .= pack "extra field is required." ]

removeUserExtraAPIHandler :: User -> ActionM ()
removeUserExtraAPIHandler (User { getUserID = uid, getUserExtra = oev }) = do
  extra <- param "extra"
  case decode extra :: Maybe Value of
    Just ev -> void (lift $ updateUserExtra uid $ differenceExtra oev ev) >> resultOK
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
  json $ object [ "from" .= from, "size" .= size, "total" .= total, "users" .= users ]

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
  json =<< lift (getBindByName name)

removeBindAPIHandler :: ActionM ()
removeBindAPIHandler = do
  bid <- param "bind_id"
  lift $ removeBind bid
  resultOK

resultOK :: ActionM ()
resultOK = json $ object [ "result" .= pack "OK" ]
