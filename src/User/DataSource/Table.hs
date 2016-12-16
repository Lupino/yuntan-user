{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Table
  (
    createTable
  ) where

import           Database.MySQL.Simple (Connection, execute_)

import           Data.Int              (Int64)
import           Data.String           (fromString)

import           User.Types


createUserTable :: TablePrefix -> Connection -> IO Int64
createUserTable prefix conn = execute_ conn sql
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

createBindTable :: TablePrefix -> Connection -> IO Int64
createBindTable prefix conn = execute_ conn sql
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

createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn = sum <$> mapM (\o -> o prefix conn) [createUserTable, createBindTable]
