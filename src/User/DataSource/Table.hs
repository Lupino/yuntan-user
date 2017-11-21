{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Table
  (
    mergeData
  ) where

import           Database.MySQL.Simple (execute_)
import           Yuntan.Types.HasMySQL (MySQL, VersionList, mergeDatabase)

import           Control.Monad         (void)
import           Data.String           (fromString)


createUserTable :: MySQL ()
createUserTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_users` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `username` varchar(128) NOT NULL,"
                                  , "  `password` varchar(128) NOT NULL,"
                                  , "  `extra` TEXT DEFAULT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `username` (`username`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createBindTable :: MySQL ()
createBindTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_binds` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `user_id` int(10) unsigned NOT NULL,"
                                  , "  `service` varchar(128) NOT NULL,"
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `extra` TEXT DEFAULT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `name` (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createGroupTable :: MySQL ()
createGroupTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_groups` ("
                                  , "  `user_id` int(10) unsigned NOT NULL,"
                                  , "  `group` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`user_id`,`group`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

updateTable_1511230647 :: MySQL ()
updateTable_1511230647 prefix conn =
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_groups`"
    , " MODIFY COLUMN `group` varchar(128) NOT NULL"
    ]

versionList :: VersionList
versionList =
  [ (1, [createUserTable, createBindTable])
  , (2, [createGroupTable])
  , (3, [updateTable_1511230647])
  ]

mergeData :: MySQL ()
mergeData = mergeDatabase versionList
