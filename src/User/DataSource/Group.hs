module User.DataSource.Group
  (
    addGroup
  , removeGroup
  , removeGroupListByUserId
  , getUserIdListByGroup
  , getGroupListByUserId
  , countGroup

  , saveGroupMeta
  , getGroupMeta
  , removeGroupMeta
  , getGroupMetaList
  ) where

import           Control.Monad           (void)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.UnixTime
import           Database.MySQL.Simple   (Only (..), execute, query, query_)
import           Yuntan.Types.HasMySQL   (MySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

import           Data.String             (fromString)
import           User.Types              (GroupMeta, GroupName, GroupSummary,
                                          GroupTitle, UserID)

addGroup :: GroupName -> UserID -> MySQL ()
addGroup group uid prefix conn = void $ execute conn sql (group, uid)
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_groups` (`group`, `user_id`) values (?, ?)" ]

removeGroup :: String -> UserID -> MySQL Int64
removeGroup group uid prefix conn = execute conn sql (group, uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_groups` WHERE `group` = ? AND `user_id` = ?" ]

getGroupListByUserId :: UserID -> MySQL [GroupName]
getGroupListByUserId uid prefix conn = map fromOnly <$> query conn sql (Only uid)
  where sql = fromString $ concat [ "SELECT `group` FROM `", prefix, "_groups` WHERE `user_id` = ?" ]

getUserIdListByGroup :: GroupName -> From -> Size -> OrderBy -> MySQL [UserID]
getUserIdListByGroup group f s o prefix conn = map fromOnly <$> query conn sql (group, f, s)
  where sql = fromString $ concat [ "SELECT `user_id` FROM `", prefix, "_groups`"
                                  , " WHERE `group` = ? "
                                  , show o, " LIMIT ?,?"
                                  ]

countGroup :: GroupName -> MySQL Int64
countGroup group prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only group)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_groups` WHERE `group` = ?" ]

removeGroupListByUserId :: UserID -> MySQL Int64
removeGroupListByUserId uid prefix conn = execute conn sql (Only uid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_groups` WHERE `user_id` = ?" ]


saveGroupMeta :: GroupName -> GroupTitle -> GroupSummary -> MySQL Int64
saveGroupMeta name title summary prefix conn = do
  old <- getGroupMeta name prefix conn
  case old of
    Nothing -> do
      t <- getUnixTime
      execute conn insertSql (name, title, summary, show $ toEpochTime t)
    Just _ -> execute conn updateSql (title, summary, name)
  where insertSql = fromString $ concat [ "INSERT INTO `", prefix, "_group_meta` (`group`, `title`, `summary`, created_at) values (?, ?, ?, ?)" ]
        updateSql = fromString $ concat [ "UPDATE `", prefix, "_group_meta` SET `title` = ?, `summary` = ? WHERE `group` = ?" ]

getGroupMeta :: GroupName -> MySQL (Maybe GroupMeta)
getGroupMeta name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_group_meta` WHERE `group`=?" ]

removeGroupMeta :: GroupName -> MySQL Int64
removeGroupMeta name prefix conn = execute conn sql (Only name)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_group_meta` WHERE `group`=?" ]

getGroupMetaList :: MySQL [GroupMeta]
getGroupMetaList prefix conn = query_ conn sql
  where sql = fromString $ concat [ "SELECT * FROM `", prefix, "_group_meta`" ]
