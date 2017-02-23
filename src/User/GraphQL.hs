{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module User.GraphQL
  (
    schema
  ) where

import           Control.Applicative    (Alternative (..))
import qualified Data.Aeson             as A (Value (..))
import           Data.GraphQL.AST       (Name)
import           Data.GraphQL.Schema    (Argument (..), Resolver, Schema (..),
                                         Value (..), array, arrayA', objectA',
                                         scalar, scalarA)
import           Data.Int               (Int32)
import           Dispatch.Types.OrderBy (desc)
import           Dispatch.Utils.GraphQL (getValue, value, value')
import           User.API
import           User.Types
import           User.UserEnv           (UserM)

schema :: Schema UserM
schema = Schema [user, bind, users, total]

user :: Resolver UserM
user = objectA' "user" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] user_ <$> getUserByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] user_ <$> getUserByName name
  (Argument "id" (ValueInt uid):_)       -> maybe [] user_ <$> getUser (fromIntegral uid)
  _ -> empty

user_ :: User -> [Resolver UserM]
user_ User{..} = [ scalar "id"         $ getUserID
                 , scalar "name"       $ getUserName
                 , value  "extra"      $ getUserExtra
                 , array  "binds"      $ map bind_ getUserBinds
                 , scalar "created_at" $ getUserCreatedAt
                 ]

bind_ :: Bind -> [Resolver UserM]
bind_ Bind{..} = [ scalar "id" getBindID
                 , scalar "user_id" getBindUid
                 , scalar "name" getBindName
                 , scalar "service" getBindService
                 , scalar "extra" getBindExtra
                 , scalar "created_at" getBindCreatedAt
                 ]

bind :: Resolver UserM
bind = objectA' "bind" $ \case
  (Argument "name" (ValueString name):_) -> maybe [] bind_ <$> getBindByName name
  (Argument "name" (ValueEnum name):_)   -> maybe [] bind_ <$> getBindByName name
  _ -> empty

users :: Resolver UserM
users = arrayA' "users" $ \ argv -> do
  case (getValue "from" argv, getValue "size" argv) of
    (Just (ValueInt f), Just (ValueInt s)) -> users_ f s
    (Just (ValueInt f), _)                 -> users_ f 10
    (_, Just (ValueInt s))                 -> users_ 0 s
    _                                      -> empty

  where users_ :: Int32 -> Int32 -> UserM [[Resolver UserM]]
        users_ f s = map user_ <$> getUsers (fromIntegral f) (fromIntegral s) (desc "id")

total :: Resolver UserM
total = scalarA "total" $ \case
  [] -> countUser
  _  -> empty
