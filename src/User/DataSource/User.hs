{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.User
  (
    createUser
  , getUser
  , getUserByName
  , getUserIdByName
  , removeUser
  , updateUserName
  , updateUserPassword
  , updateUserExtra
  , updateUserSecureExtra
  , countUser
  , getUsers
  , getUserIdList
  ) where

import           Control.Monad           (void)
import           Database.MySQL.Simple   (Only (..), execute, insertID, query,
                                          query_)
import           Yuntan.Types.HasMySQL   (MySQL)

import           Data.Aeson              (encode)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime

import           User.Types
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createUser :: UserName -> Password -> MySQL UserID
createUser name passwd prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, passwd, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_users` "
                                  , "(`username`, `password`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?)"
                                  ]

getUser :: UserID -> MySQL (Maybe User)
getUser uid prefix conn = listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` WHERE `id`=?"]

getUserByName :: UserName -> MySQL (Maybe User)
getUserByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` WHERE `username`=?"]

getUserIdByName :: UserName -> MySQL (Maybe UserID)
getUserIdByName name prefix conn =
  maybe Nothing (Just . fromOnly) . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_users` WHERE `username`=?"]

removeUser :: UserID -> MySQL Int64
removeUser uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_users` WHERE `id`=?"]

updateUserName :: UserID -> UserName -> MySQL Int64
updateUserName uid name prefix conn = execute conn sql (name, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `username` = ? WHERE `id`=?"]

updateUserPassword :: UserID -> Password -> MySQL Int64
updateUserPassword uid passwd prefix conn = execute conn sql (passwd, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `password` = ? WHERE `id`=?"]

updateUserExtra :: UserID -> Extra -> MySQL Int64
updateUserExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `extra` = ? WHERE `id`=?"]

updateUserSecureExtra :: UserID -> Extra -> MySQL Int64
updateUserSecureExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `secure_extra` = ? WHERE `id`=?"]

countUser :: MySQL Int64
countUser prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_users`" ]

getUsers :: From -> Size -> OrderBy -> MySQL [User]
getUsers from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` ", show o, " LIMIT ?,?" ]

getUserIdList :: From -> Size -> OrderBy -> MySQL [UserID]
getUserIdList from size o prefix conn = map fromOnly <$> query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT `id` FROM `", prefix, "_users` ", show o, " LIMIT ?,?" ]
