{-# LANGUAGE OverloadedStrings #-}

module User.DataSource.Bind
  ( createBind
  , getBind
  , getBindIdByName
  , removeBind
  , updateBindExtra
  , countBindByUID
  , getBindIdListByUID
  , removeBindByUID
  , countBindByUIDAndService
  , getBindIdListByUIDAndService
  , countBindByService
  , getBindIdListByService
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.UnixTime
import           Database.PSQL.Types    (From, Only (..), OrderBy, PSQL, Size,
                                         count, delete, insertRet, selectOne,
                                         selectOneOnly, selectOnly, update)
import           User.DataSource.Table  (binds)
import           User.Types

createBind :: UserID -> Service -> ServiceName -> Extra -> PSQL BindID
createBind uid service name extra = do
  t <- liftIO getUnixTime
  insertRet binds ["user_id", "service", "name", "extra", "created_at"] "id"
    (uid, service, name, extra, show $ toEpochTime t) 0

getBind :: BindID -> PSQL (Maybe Bind)
getBind bid = selectOne binds ["*"] "id = ?" (Only bid)

getBindIdByName :: ServiceName -> PSQL (Maybe BindID)
getBindIdByName name = selectOneOnly binds "id" "name = ?" (Only name)

removeBind :: BindID -> PSQL Int64
removeBind bid = delete binds "id = ?" (Only bid)

updateBindExtra :: BindID -> Extra -> PSQL Int64
updateBindExtra bid extra = update binds ["extra"] "id = ?" (extra, bid)

countBindByUID :: UserID -> PSQL Int64
countBindByUID uid = count binds "user_id = ?" (Only uid)

getBindIdListByUID :: UserID -> From -> Size -> OrderBy -> PSQL [BindID]
getBindIdListByUID uid = selectOnly binds  "id" "user_id = ?" (Only uid)

countBindByService :: Service -> PSQL Int64
countBindByService service = count binds "service = ?" (Only service)

getBindIdListByService :: Service -> From -> Size -> OrderBy -> PSQL [BindID]
getBindIdListByService service = selectOnly binds "id" "service = ?" (Only service)

countBindByUIDAndService :: UserID -> Service -> PSQL Int64
countBindByUIDAndService uid service = count binds "user_id = ? AND service = ?" (uid, service)

getBindIdListByUIDAndService :: UserID -> Service -> From -> Size -> OrderBy -> PSQL [BindID]
getBindIdListByUIDAndService uid service = selectOnly binds "id" "user_id = ? AND service = ?" (uid, service)

removeBindByUID :: UserID -> PSQL Int64
removeBindByUID uid = delete binds "user_id = ?" (Only uid)
