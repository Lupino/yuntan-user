{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Table
  ( mergeData
  , users
  , binds
  , groups
  , groupMeta
  ) where

import           Data.Int             (Int64)
import           Yuntan.Types.HasPSQL (PSQL, TableName, VersionList,
                                       constraintPrimaryKey, createIndex,
                                       mergeDatabase)
import qualified Yuntan.Types.HasPSQL as PSQL (createTable)

users :: TableName
users = "users"


binds :: TableName
binds = "binds"


groups :: TableName
groups = "groups"


groupMeta :: TableName
groupMeta = "group_meta"


createUserTable :: PSQL Int64
createUserTable =
  PSQL.createTable users
    [ "id SERIAL PRIMARY KEY"
    , "username VARCHAR(128) NOT NULL"
    , "password VARCHAR(128) NOT NULL"
    , "extra JSON"
    , "secure_extra JSON"
    , "created_at INT NOT NULL"
    ]


createBindTable :: PSQL Int64
createBindTable =
  PSQL.createTable binds
    [ "id SERIAL PRIMARY KEY"
    , "user_id INT NOT NULL"
    , "service VARCHAR(128) NOT NULL"
    , "name VARCHAR(128) NOT NULL"
    , "extra JSON"
    , "created_at INT NOT NULL"
    ]


createGroupTable :: PSQL Int64
createGroupTable prefix =
  PSQL.createTable groups
    [ "user_id INT NOT NULL"
    , "\"group\" VARCHAR(128) NOT NULL"
    , constraintPrimaryKey prefix "group_pk" ["user_id", "\"group\""]
    ] prefix


createGroupMetaTable :: PSQL Int64
createGroupMetaTable prefix =
  PSQL.createTable groupMeta
    [ "\"group\" VARCHAR(128) NOT NULL"
    , "title VARCHAR(256) NOT NULL"
    , "summary VARCHAR(1500) DEFAULT NULL"
    , "created_at INT NOT NULL"
    , constraintPrimaryKey prefix "group_meta_pk" ["\"group\""]
    ] prefix


versionList :: VersionList Int64
versionList =
  [ (1, [ createUserTable
        , createIndex True users "username" ["username"]
        , createBindTable
        , createIndex True binds "name" ["name"]
        , createIndex False binds "user_id" ["user_id"]
        , createIndex False binds "service" ["service"]
        , createIndex False binds "user_service" ["user_id", "service"]
        ]
    )
  , (2, [createGroupTable])
  , (7, [createGroupMetaTable])
  ]

mergeData :: PSQL ()
mergeData = mergeDatabase versionList
