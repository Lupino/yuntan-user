{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.User
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
  ) where

import           Control.Monad           (void)
import           Database.MySQL.Simple   (Connection, Only (..), execute,
                                          insertID, query, query_)

import           Data.Aeson              (encode)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime

import           User.Types
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createUser :: UserName -> Password -> TablePrefix -> Connection -> IO UserID
createUser name passwd prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, passwd, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where sql = fromString $ concat [ "INSERT INTO `", prefix, "_users` "
                                  , "(`username`, `password`, `created_at`)"
                                  , " VALUES "
                                  , "(?, ?, ?)"
                                  ]

getUser :: UserID -> TablePrefix -> Connection -> IO (Maybe User)
getUser uid prefix conn = listToMaybe <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` WHERE `id`=?"]

getUserByName :: UserName -> TablePrefix -> Connection -> IO (Maybe User)
getUserByName name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` WHERE `username`=?"]

removeUser :: UserID -> TablePrefix -> Connection -> IO Int64
removeUser uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_users` WHERE `id`=?"]

updateUserName :: UserID -> UserName -> TablePrefix -> Connection -> IO Int64
updateUserName uid name prefix conn = execute conn sql (name, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `username` = ? WHERE `id`=?"]

updateUserPassword :: UserID -> Password -> TablePrefix -> Connection -> IO Int64
updateUserPassword uid passwd prefix conn = execute conn sql (passwd, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `password` = ? WHERE `id`=?"]

updateUserExtra :: UserID -> Extra -> TablePrefix -> Connection -> IO Int64
updateUserExtra uid extra prefix conn = execute conn sql (encode extra, uid)
  where sql = fromString $ concat [ "UPDATE `", prefix, "_users` SET `extra` = ? WHERE `id`=?"]

countUser :: TablePrefix -> Connection -> IO Int64
countUser prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_users`" ]

getUsers :: From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [User]
getUsers from size o prefix conn = query conn sql (from, size)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_users` ", show o, " LIMIT ?,?" ]
