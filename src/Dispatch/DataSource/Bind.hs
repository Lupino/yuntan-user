{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DataSource.Bind
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

import           Database.MySQL.Simple (Connection, Only (..), execute,
                                        insertID, query, query_)

import           Data.Aeson            (Value (..), encode)
import           Data.Int              (Int64)
import           Data.Maybe            (listToMaybe)
import           Data.String           (fromString)
import           Data.UnixTime

import           Dispatch.Types

createBind :: UserID -> Service -> ServiceName -> Extra -> TablePrefix -> Connection -> IO BindID
createBind uid service name extra prefix conn = do
  t <- getUnixTime
  execute conn sql (uid, service, name, encode extra, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_binds` "
                                  , "(`user_id`, `service`, `name`, `extra`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?, ?, ?)"
                                  ]

getBind :: BindID -> TablePrefix -> Connection -> IO (Maybe Bind)
getBind uid prefix conn = listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `id`=?"]

getBindByName :: ServiceName -> TablePrefix -> Connection -> IO (Maybe Bind)
getBindByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `name`=?"]

removeBind :: BindID -> TablePrefix -> Connection -> IO Int64
removeBind uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `id`=?"]

updateBindExtra :: BindID -> Extra -> TablePrefix -> Connection -> IO Int64
updateBindExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_binds` SET `extra` = ? WHERE `id`=?"]

countBind :: UserID -> TablePrefix -> Connection -> IO Int64
countBind uid prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_binds` WHERE `user_id`=?" ]

getBinds :: UserID -> TablePrefix -> Connection -> IO [Bind]
getBinds uid prefix conn = query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_binds` WHERE `user_id`=?" ]

removeBinds :: UserID -> TablePrefix -> Connection -> IO Int64
removeBinds uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `user_id`=?" ]
