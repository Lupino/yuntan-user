{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Group
  ( addGroup
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

import           Data.Int                   (Int64)
import           Data.UnixTime
import           Database.PostgreSQL.Simple (Only (..))
import           User.DataSource.Table      (groupMeta, groups)
import           User.Types                 (GroupMeta, GroupName, GroupSummary,
                                             GroupTitle, UserID)
import           Yuntan.Types.HasPSQL       (PSQL, count, delete,
                                             insertOrUpdate, selectOne,
                                             selectOnly, select_)
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy, emptyOrder)

addGroup :: GroupName -> UserID -> PSQL Int64
addGroup group uid = insertOrUpdate groups ["group", "user_id"] [] [] (group, uid)

removeGroup :: GroupName -> UserID -> PSQL Int64
removeGroup group uid = delete groups "group = ? AND user_id = ?" (group, uid)

getGroupListByUserId :: UserID -> PSQL [GroupName]
getGroupListByUserId uid = selectOnly groups "group" "user_id = ?" (Only uid) 0 1000 emptyOrder

getUserIdListByGroup :: GroupName -> From -> Size -> OrderBy -> PSQL [UserID]
getUserIdListByGroup group = selectOnly groups "user_id" "group = ?" (Only group)

countGroup :: GroupName -> PSQL Int64
countGroup group = count groups "group = ?" (Only group)

removeGroupListByUserId :: UserID -> PSQL Int64
removeGroupListByUserId uid = delete groups "user_id = ?" (Only uid)

saveGroupMeta :: GroupName -> GroupTitle -> GroupSummary -> PSQL Int64
saveGroupMeta name title summary prefix conn = do
  t <- getUnixTime
  insertOrUpdate groupMeta ["group"] ["title", "summary"] ["created_at"]
    (name, title, summary, show $ toEpochTime t) prefix conn

getGroupMeta :: GroupName -> PSQL (Maybe GroupMeta)
getGroupMeta name = selectOne groupMeta ["*"] "group = ?" (Only name)

removeGroupMeta :: GroupName -> PSQL Int64
removeGroupMeta name = delete groupMeta "group = ?" (Only name)

getGroupMetaList :: PSQL [GroupMeta]
getGroupMetaList = select_ groups ["*"] 0 1000 emptyOrder
