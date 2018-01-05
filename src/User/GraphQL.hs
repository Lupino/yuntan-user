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
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Haxl.Core             (GenHaxl)
import           User.API
import           User.Types
import           Yuntan.Types.HasMySQL (HasMySQL)
import           Yuntan.Types.OrderBy  (desc)
import           Yuntan.Utils.GraphQL  (getTextValue, getValue, value)

-- type Query {
--  user(name: String!): User
--  user(name: Enum!): User
--  user(id: Int!): User
--  bind(name: String!): Bind
--  bind(name: Enum!): Bind
--  service_binds(service: String!, from: Int, size: Int): [Bind]
--  service_bind_count(service: String!)
--  users(from: Int, size: Int): [User]
--  total: Int
-- }
-- type User {
--  id: Int
--  name: String
--  extra: Extra
--  binds(from: Int, size: Int): [Bind]
--  bind_count: Int
--  groups: [String]
--  service_binds(service: String!, from: Int, size: Int): [Bind]
--  service_bind_count(service: String!): Int
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
schema = user :| [bind, users, total, serviceBinds', serviceBindCount']

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
                 , binds "binds"       getUserID
                 , bindCount "bind_count" getUserID
                 , serviceBinds "service_binds" getUserID
                 , serviceBindCount "service_bind_count" getUserID
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

binds :: HasMySQL u => Name -> UserID -> Resolver (GenHaxl u)
binds n uid = arrayA' n $ \argv ->
  case (getValue "from" argv, getValue "size" argv) of
    (Just (ValueInt f), Just (ValueInt s)) -> binds_ f s
    (Just (ValueInt f), _)                 -> binds_ f 10
    (_, Just (ValueInt s))                 -> binds_ 0 s
    _                                      -> empty

  where binds_ :: HasMySQL u => Int32 -> Int32 -> GenHaxl u [[Resolver (GenHaxl u)]]
        binds_ f s = map bind_ <$> getBindListByUID uid (fromIntegral f) (fromIntegral s) (desc "id")

bindCount :: HasMySQL u => Name -> UserID -> Resolver (GenHaxl u)
bindCount n uid = scalarA n $ \case
  [] -> countBindByUID uid
  _  -> empty

serviceBinds :: HasMySQL u => Name -> UserID -> Resolver (GenHaxl u)
serviceBinds n uid = arrayA' n $ \argv ->
  case getTextValue "service" argv of
    Nothing -> empty
    Just srv ->
      case (getValue "from" argv, getValue "size" argv) of
        (Just (ValueInt f), Just (ValueInt s)) -> binds_ srv f s
        (Just (ValueInt f), _)                 -> binds_ srv f 10
        (_, Just (ValueInt s))                 -> binds_ srv 0 s
        _                                      -> empty

  where binds_ :: HasMySQL u => Text -> Int32 -> Int32 -> GenHaxl u [[Resolver (GenHaxl u)]]
        binds_ srv f s = map bind_ <$> getBindListByUIDAndService uid srv (fromIntegral f) (fromIntegral s) (desc "id")

serviceBindCount :: HasMySQL u => Name -> UserID -> Resolver (GenHaxl u)
serviceBindCount n uid = scalarA n $ \argv ->
  case getTextValue "service" argv of
    Just srv -> countBindByUIDAndService uid srv
    Nothing  -> empty

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

serviceBinds' :: HasMySQL u => Resolver (GenHaxl u)
serviceBinds' = arrayA' "service_binds" $ \argv ->
  case getTextValue "service" argv of
    Nothing -> empty
    Just srv ->
      case (getValue "from" argv, getValue "size" argv) of
        (Just (ValueInt f), Just (ValueInt s)) -> binds_ srv f s
        (Just (ValueInt f), _)                 -> binds_ srv f 10
        (_, Just (ValueInt s))                 -> binds_ srv 0 s
        _                                      -> empty

  where binds_ :: HasMySQL u => Text -> Int32 -> Int32 -> GenHaxl u [[Resolver (GenHaxl u)]]
        binds_ srv f s = map bind_ <$> getBindListByService srv (fromIntegral f) (fromIntegral s) (desc "id")

serviceBindCount' :: HasMySQL u => Resolver (GenHaxl u)
serviceBindCount' = scalarA "service_bind_count" $ \argv ->
  case getTextValue "service" argv of
    Just srv -> countBindByService srv
    Nothing  -> empty
