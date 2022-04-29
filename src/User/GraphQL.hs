{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module User.GraphQL
  ( schema
  , schemaByUser
  , schemaByBind
  , schemaByService
  ) where

import           Control.Applicative (Alternative (..))
import           Data.Aeson.Helper   (union)
import           Data.GraphQL.AST    (Name)
import           Data.GraphQL.Schema (Argument (..), Resolver, Schema,
                                      Value (..), arrayA', object', objectA',
                                      scalar, scalarA)
import           Data.GraphQL.Utils  (getInt, getText, pick, value)
import           Data.List.NonEmpty  (NonEmpty ((:|)), fromList)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Database.PSQL.Types (From, HasOtherEnv, HasPSQL, Size, desc)
import           Haxl.Core           (GenHaxl, throw)
import           Haxl.Prelude        (NotFound (..), catchAny)
import           User.API
import           User.Config         (Cache)
import           User.Types


instance Alternative (GenHaxl u w) where
  a <|> b = catchAny a b
  empty = throw $ NotFound "mzero"

-- type Query {
--  user(name: String!): User
--  user(name: Enum!): User
--  user(id: Int!): User
--  bind(name: String!): Bind
--  bind(name: Enum!): Bind
--  service(service: String!): Service
--  service_binds(service: String!, from: Int, size: Int): [Bind]
--  service_bind_count(service: String!)
--  users(from: Int, size: Int): [User]
--  user_count: Int
--  group(group: String): Group
-- }
-- type Service {
--  service: String
--  binds(from: Int, size: Int): [Bind]
--  bind_count: Int
-- }
-- type Group {
--  group: String
--  users(from: Int, size: Int): [User]
--  user_count: Int
-- }
-- type User {
--  id: Int
--  name: String
--  extra: Extra
--  pick_extra(keys: [String]): Extra
--  binds(from: Int, size: Int): [Bind]
--  bind_count: Int
--  groups: [String]
--  service(service: String!): Service
--  created_at: Int
-- }
-- type Bind {
--  id: Int
--  user_id: Int
--  user: User
--  name: String
--  service: String
--  extra: Extra
--  pick_extra(keys: [String]): Extra
--  created_at: Int
-- }
-- type Extra {
--
-- }

schema :: (HasPSQL u, HasOtherEnv Cache u) => Schema (GenHaxl u w)
schema = user :| [bind, users, userCount, service, group]

schemaByUser :: (HasPSQL u, HasOtherEnv Cache u) => User -> Schema (GenHaxl u w)
schemaByUser u = fromList (user_ u)

schemaByBind :: (HasPSQL u, HasOtherEnv Cache u) => Bind -> Schema (GenHaxl u w)
schemaByBind b = fromList (bind_ b)

schemaByService :: (HasPSQL u, HasOtherEnv Cache u) => Service -> Schema (GenHaxl u w)
schemaByService b = fromList (service_ b)

user :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
user = objectA' "user" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] user_ <$> getUserByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] user_ <$> getUserByName name
  (Argument "id" (ValueInt uid):_)       -> maybe [] user_ <$> getUser (fromIntegral uid)
  _ -> empty

user_ :: (HasPSQL u, HasOtherEnv Cache u) => User -> [Resolver (GenHaxl u w)]
user_ User{..} =
  [ scalar    "id"         getUserID
  , scalar    "name"       getUserName
  , value     "extra"      $ union getUserSecureExtra getUserExtra
  , pick      "pick_extra" $ union getUserSecureExtra getUserExtra
  , binds     "binds"      getUserID
  , bindCount "bind_count" getUserID
  , service'  "service"    getUserID
  , scalar    "groups"     getUserGroups
  , scalar    "created_at" getUserCreatedAt
  ]

bind_ :: (HasPSQL u, HasOtherEnv Cache u) => Bind -> [Resolver (GenHaxl u w)]
bind_ Bind{..} =
  [ scalar "id"         getBindID
  , scalar "user_id"    getBindUid
  , user__ "user"       getBindUid
  , scalar "name"       getBindName
  , scalar "service"    getBindService
  , value  "extra"      getBindExtra
  , pick   "pick_extra" getBindExtra
  , scalar "created_at" getBindCreatedAt
  ]

user__ :: (HasPSQL u, HasOtherEnv Cache u) => Name -> UserID -> Resolver (GenHaxl u w)
user__ n uid = object' n $ maybe [] user_ <$> getUser uid

bind :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
bind = objectA' "bind" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] bind_ <$> getBindByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] bind_ <$> getBindByName name
  _ -> empty

paramPage :: [Argument] -> (From, Size)
paramPage argv = (from , size)
  where from = fromMaybe 0  $ getInt "from" argv
        size = fromMaybe 10 $ getInt "size" argv

binds :: (HasPSQL u, HasOtherEnv Cache u) => Name -> UserID -> Resolver (GenHaxl u w)
binds n uid = arrayA' n $ \argv ->
  let (f, s) = paramPage argv
      in map bind_ <$> getBindListByUID uid f s (desc "id")

bindCount :: HasPSQL u => Name -> UserID -> Resolver (GenHaxl u w)
bindCount n uid = scalarA n $ \case
  [] -> countBindByUID uid
  _  -> empty

service :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
service = objectA' "service" $ \argv ->
  case getText "service" argv of
    Just srv -> pure $ service_ srv
    Nothing  -> empty

service' :: (HasPSQL u, HasOtherEnv Cache u) => Name -> UserID -> Resolver (GenHaxl u w)
service' n uid = objectA' n $ \argv ->
  case getText "service" argv of
    Just srv -> pure $ service__ uid srv
    Nothing  -> empty

service_ :: (HasPSQL u, HasOtherEnv Cache u) => Service -> [Resolver (GenHaxl u w)]
service_ srv = [ scalar "service" srv
               , serviceBinds "binds" srv
               , serviceBindCount "bind_count" srv
               ]

service__ :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> Service -> [Resolver (GenHaxl u w)]
service__ uid srv = [ scalar "service" srv
                    , serviceBinds_ "binds" uid srv
                    , serviceBindCount_ "bind_count" uid srv
                    ]

serviceBinds :: (HasPSQL u, HasOtherEnv Cache u) => Name -> Service -> Resolver (GenHaxl u w)
serviceBinds n srv = arrayA' n $ \argv ->
  let (f, s) = paramPage argv
      in map bind_ <$> getBindListByService srv f s (desc "id")

serviceBindCount :: HasPSQL u => Name -> Service -> Resolver (GenHaxl u w)
serviceBindCount n srv = scalarA n $ \_ -> countBindByService srv

serviceBinds_ :: (HasPSQL u, HasOtherEnv Cache u) => Name -> UserID -> Service -> Resolver (GenHaxl u w)
serviceBinds_ n uid srv = arrayA' n $ \argv ->
  let (f, s) = paramPage argv
      in map bind_ <$> getBindListByUIDAndService uid srv f s (desc "id")

serviceBindCount_ :: HasPSQL u => Name -> UserID -> Service -> Resolver (GenHaxl u w)
serviceBindCount_ n uid srv = scalarA n $ \_ -> countBindByUIDAndService uid srv

users :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
users = arrayA' "users" $ \argv ->
  let (f, s) = paramPage argv
      in map user_ <$> getUsers f s (desc "id")

userCount :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
userCount = scalarA "user_count" $ \case
  [] -> countUser
  _  -> empty

group :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
group = objectA' "group" $ \argv ->
  case getText "group" argv of
    Just srv -> pure $ group_ srv
    Nothing  -> empty

group_ :: (HasPSQL u, HasOtherEnv Cache u) => Text -> [Resolver (GenHaxl u w)]
group_ g = [ scalar "group" g
           , groupUsers "users" g
           , groupUserCount "user_count" g
           ]

groupUsers :: (HasPSQL u, HasOtherEnv Cache u) => Name -> Text -> Resolver (GenHaxl u w)
groupUsers n g = arrayA' n $ \argv ->
  let (f, s) = paramPage argv
      in map user_ <$> getUserListByGroup g f s (desc "user_id")

groupUserCount :: (HasPSQL u, HasOtherEnv Cache u) => Name -> Text -> Resolver (GenHaxl u w)
groupUserCount n g = scalarA n $ \_ -> countGroup g
