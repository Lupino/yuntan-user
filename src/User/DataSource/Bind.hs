{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Bind
  (
    createBind
  , getBind
  , getBindByName
  , removeBind
  , updateBindExtra
  , countBind
  , getBinds
  , removeBinds
  ) where

import           Control.Monad         (void)
import           Database.MySQL.Simple (Only (..), execute, insertID, query)
import           Yuntan.Types.HasMySQL (MySQL)

import           Data.Aeson            (encode)
import           Data.Int              (Int64)
import           Data.Maybe            (listToMaybe)
import           Data.String           (fromString)
import           Data.UnixTime

import           User.Types

createBind :: UserID -> Service -> ServiceName -> Extra -> MySQL BindID
createBind uid service name extra prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (uid, service, name, encode extra, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_binds` "
                                  , "(`user_id`, `service`, `name`, `extra`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?, ?, ?)"
                                  ]

getBind :: BindID -> MySQL (Maybe Bind)
getBind uid prefix conn = listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `id`=?"]

getBindByName :: ServiceName -> MySQL (Maybe Bind)
getBindByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `name`=?"]

removeBind :: BindID -> MySQL Int64
removeBind uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `id`=?"]

updateBindExtra :: BindID -> Extra -> MySQL Int64
updateBindExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_binds` SET `extra` = ? WHERE `id`=?"]

countBind :: UserID -> MySQL Int64
countBind uid prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_binds` WHERE `user_id`=?" ]

getBinds :: UserID -> MySQL [Bind]
getBinds uid prefix conn = query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `user_id`=?" ]

removeBinds :: UserID -> MySQL Int64
removeBinds uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `user_id`=?" ]
