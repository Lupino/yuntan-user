{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module User.GraphQL
  (
    schema
  , schemaByUser
  , schemaByBind
  ) where

import           Control.Applicative   (Alternative (..))
import           Data.GraphQL.AST      (Name)
import           Data.GraphQL.Schema   (Argument (..), Resolver, Schema,
                                        Value (..), array, arrayA', object',
                                        objectA', scalar, scalarA)
import           Data.Int              (Int32)
import           Data.List.NonEmpty    (NonEmpty ((:|)), fromList)
import           Haxl.Core             (GenHaxl)
import           User.API
import           User.Types
import           Yuntan.Types.HasMySQL (HasMySQL)
import           Yuntan.Types.OrderBy  (desc)
import           Yuntan.Utils.GraphQL  (getValue, value)

-- type Query {
--  user(name: String!): User
--  user(name: Enum!): User
--  user(id: Int!): User
--  bind(name: String!): Bind
--  bind(name: Enum!): Bind
--  users(from: Int, size: Int): [User]
--  total: Int
-- }
-- type User {
--  id: Int
--  name: String
--  extra: Extra
--  binds: [Bind]
--  groups: [String]
--  created_at: Int
-- }
-- type Bind {
--  id: Int
--  user_id: Int
--  user: User
--  name: String
--  service: String
--  extra: Extra
--  created_at: Int
-- }
-- type Extra {
--
-- }

schema :: HasMySQL u => Schema (GenHaxl u)
schema = user :| [bind, users, total]

schemaByUser :: HasMySQL u => User -> Schema (GenHaxl u)
schemaByUser u = fromList (user_ u)

schemaByBind :: HasMySQL u => Bind -> Schema (GenHaxl u)
schemaByBind b = fromList (bind_ b)

user :: HasMySQL u => Resolver (GenHaxl u)
user = objectA' "user" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] user_ <$> getUserByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] user_ <$> getUserByName name
  (Argument "id" (ValueInt uid):_)       -> maybe [] user_ <$> getUser (fromIntegral uid)
  _ -> empty

user_ :: HasMySQL u => User -> [Resolver (GenHaxl u)]
user_ User{..} = [ scalar "id"         getUserID
                 , scalar "name"       getUserName
                 , value  "extra"      getUserExtra
                 , array  "binds"      $ map bind_ getUserBinds
                 , scalar "groups"     getUserGroups
                 , scalar "created_at" getUserCreatedAt
                 ]

bind_ :: HasMySQL u => Bind -> [Resolver (GenHaxl u)]
bind_ Bind{..} = [ scalar "id" getBindID
                 , scalar "user_id" getBindUid
                 , user__ "user" getBindUid
                 , scalar "name" getBindName
                 , scalar "service" getBindService
                 , scalar "extra" getBindExtra
                 , scalar "created_at" getBindCreatedAt
                 ]

user__ :: HasMySQL u => Name -> UserID -> Resolver (GenHaxl u)
user__ n uid = object' n $ maybe [] user_ <$> getUser uid

bind :: HasMySQL u => Resolver (GenHaxl u)
bind = objectA' "bind" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] bind_ <$> getBindByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] bind_ <$> getBindByName name
  _ -> empty

users :: HasMySQL u => Resolver (GenHaxl u)
users = arrayA' "users" $ \ argv ->
  case (getValue "from" argv, getValue "size" argv) of
    (Just (ValueInt f), Just (ValueInt s)) -> users_ f s
    (Just (ValueInt f), _)                 -> users_ f 10
    (_, Just (ValueInt s))                 -> users_ 0 s
    _                                      -> empty

  where users_ :: HasMySQL u => Int32 -> Int32 -> GenHaxl u [[Resolver (GenHaxl u)]]
        users_ f s = map user_ <$> getUsers (fromIntegral f) (fromIntegral s) (desc "id")

total :: HasMySQL u => Resolver (GenHaxl u)
total = scalarA "total" $ \case
  [] -> countUser
  _  -> empty
