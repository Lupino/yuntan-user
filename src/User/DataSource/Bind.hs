{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Bind
  (
    createBind
  , getBind
  , getBindIdByName
  , removeBind
  , updateBindExtra
  , countBindByUID
  , getBindIdListByUID
  , removeBindByUID
  , countBindByUIDAndService
  , getBindIdListByUIDAndService
  , countBindByService
  , getBindIdListByService
  ) where

import           Control.Monad           (void)
import           Database.MySQL.Simple   (Only (..), execute, insertID, query)
import           Yuntan.Types.HasMySQL   (MySQL)

import           Data.Aeson              (encode)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

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

getBindIdByName :: ServiceName -> MySQL (Maybe BindID)
getBindIdByName name prefix conn =
  fmap fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_binds` WHERE `name`=?"]

removeBind :: BindID -> MySQL Int64
removeBind uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `id`=?"]

updateBindExtra :: BindID -> Extra -> MySQL Int64
updateBindExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_binds` SET `extra` = ? WHERE `id`=?"]

countBindByUID :: UserID -> MySQL Int64
countBindByUID uid prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_binds` WHERE `user_id`=?" ]

getBindIdListByUID :: UserID -> From -> Size -> OrderBy -> MySQL [BindID]
getBindIdListByUID uid from size o prefix conn = map fromOnly <$> query conn sql (uid, from, size)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_binds`"
                                  , " WHERE `user_id`=? "
                                  , show o, " LIMIT ?,?" ]

countBindByService :: Service -> MySQL Int64
countBindByService service prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (Only service)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_binds` WHERE `service`=?" ]

getBindIdListByService :: Service -> From -> Size -> OrderBy -> MySQL [BindID]
getBindIdListByService service from size o prefix conn =
  map fromOnly <$> query conn sql (service, from, size)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_binds`"
                                  , " WHERE `service`=? "
                                  , show o, " LIMIT ?,?" ]

countBindByUIDAndService :: UserID -> Service -> MySQL Int64
countBindByUIDAndService uid service prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (uid, service)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_binds`"
                                  , " WHERE `user_id`=? AND `service`=?" ]

getBindIdListByUIDAndService :: UserID -> Service -> From -> Size -> OrderBy -> MySQL [BindID]
getBindIdListByUIDAndService uid service from size o prefix conn =
  map fromOnly <$> query conn sql (uid, service, from, size)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_binds`"
                                  , " WHERE `user_id`=? AND `service`=? "
                                  , show o, " LIMIT ?,?" ]

removeBindByUID :: UserID -> MySQL Int64
removeBindByUID uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_binds` WHERE `user_id`=?" ]
