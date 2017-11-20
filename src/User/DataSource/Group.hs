module User.DataSource.Group
  (
    addGroup
  , removeGroup
  , removeGroupListByUserID
  , getUserIDListByGroup
  , getGroupListByUserID
  , countGroup
  ) where

import           Control.Monad           (void)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Database.MySQL.Simple   (Only (..), execute, query)
import           Yuntan.Types.HasMySQL   (MySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

import           Data.String             (fromString)
import           User.Types              (GroupName, UserID)

addGroup :: GroupName -> UserID -> MySQL ()
addGroup group uid prefix conn = void $ execute conn sql (group, uid)
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_groups` (`group`, `user_id`) values (?, ?)" ]

removeGroup :: String -> UserID -> MySQL Int64
removeGroup group uid prefix conn = execute conn sql (group, uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_groups` WHERE `group` = ? AND `user_id` = ?" ]

getGroupListByUserID :: UserID -> MySQL [GroupName]
getGroupListByUserID uid prefix conn = map fromOnly <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT `group` FROM `", prefix, "_groups` WHERE `user_id` = ?" ]

getUserIDListByGroup :: GroupName -> From -> Size -> OrderBy -> MySQL [UserID]
getUserIDListByGroup group f s o prefix conn = map fromOnly <$> query conn sql (group, f, s)
  where sql = fromString $ concat [ "SELECT `user_id` FROM `", prefix, "_groups`"
                                  , " WHERE `group` = ? "
                                  , show o, " LIMIT ?,?"
                                  ]

countGroup :: GroupName -> MySQL Int64
countGroup group prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only group)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_groups` WHERE `group` = ?" ]

removeGroupListByUserID :: UserID -> MySQL Int64
removeGroupListByUserID uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_groups` WHERE `user_id` = ?" ]
