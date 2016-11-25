{-# LANGUAGE OverloadedStrings #-}

module Dispatch.UserEnv
  (
    UserEnv(..)
  , ActionM
  , ScottyM
  , DispatchM
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Pool              (Pool)
import qualified Data.Text.Lazy         as LT (Text)
import           Database.MySQL.Simple  (Connection)
import           Haxl.Core              (GenHaxl)
import           Haxl.Core.Monad        (unsafeLiftIO)
import           Web.Scotty.Trans       (ActionT, ScottyT)

data UserEnv = UserEnv { mySQLPool   :: Pool Connection
                       , tablePrefix :: String
                       }

type DispatchM = GenHaxl UserEnv

instance MonadIO (GenHaxl u) where
  liftIO = unsafeLiftIO

type ActionM a = ActionT LT.Text DispatchM a
type ScottyM a = ScottyT LT.Text DispatchM a
