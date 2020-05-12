{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.User
  ( createUser
  , getUser
  , getUserIdByName
  , removeUser
  , updateUserName
  , updateUserPassword
  , updateUserExtra
  , updateUserSecureExtra
  , countUser
  , getUserIdList
  ) where

import           Data.Int                   (Int64)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Data.UnixTime
import           Database.PostgreSQL.Simple (Only (..))
import           Yuntan.Types.HasPSQL       (PSQL, count_, delete, insertRet,
                                             selectOne, selectOneOnly,
                                             selectOnly_, update)

import           User.DataSource.Table      (users)
import           User.Types
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy)

createUser :: UserName -> Password -> PSQL UserID
createUser name passwd prefix conn = do
  t <- getUnixTime
  insertRet users ["username", "password", "created_at"] "id"
    (name, passwd, show $ toEpochTime t) prefix conn

getUser :: UserID -> PSQL (Maybe User)
getUser uid = selectOne users ["*"] "id = ?" (Only uid)

getUserIdByName :: UserName -> PSQL (Maybe UserID)
getUserIdByName name = selectOneOnly users "id" "username = ?" (Only name)

removeUser :: UserID -> PSQL Int64
removeUser uid = delete users "id = ?" (Only uid)

updateUserName :: UserID -> UserName -> PSQL Int64
updateUserName uid name = update users ["username"] "id = ?" (name, uid)

updateUserPassword :: UserID -> Password -> PSQL Int64
updateUserPassword uid passwd = update users ["password"] "id = ?" (passwd, uid)

updateUserExtra :: UserID -> Extra -> PSQL Int64
updateUserExtra uid extra = update users ["extra"] "id = ?" (extra, uid)

updateUserSecureExtra :: UserID -> Extra -> PSQL Int64
updateUserSecureExtra uid extra = update users ["secure_extra"] "id = ?" (extra, uid)

countUser :: PSQL Int64
countUser = count_ users

getUserIdList :: From -> Size -> OrderBy -> PSQL [UserID]
getUserIdList = selectOnly_ users "id"
