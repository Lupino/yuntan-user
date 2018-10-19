{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module User.API
  ( createUser
  , getUser
  , getUserByName
  , removeUser
  , updateUserName
  , updateUserPassword
  , updateUserExtra
  , updateUserSecureExtra
  , getUsers
  , countUser

  , createBind
  , getBind
  , getBindByName
  , removeBind
  , updateBindExtra
  , getBindListByUID
  , getBindListByService
  , getBindListByUIDAndService
  , removeBindByUID

  , addGroup
  , removeGroup
  , removeGroupListByUserId
  , getUserListByGroup
  , countGroup

  , saveGroupMeta
  , removeGroupMeta
  , getGroupMeta
  , getGroupMetaList
  , module X
  ) where

import           Control.Monad.Extra     (maybeM)
import           Data.ByteString         (ByteString)
import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes)
import           Data.String             (fromString)
import qualified Data.Text               as T (unpack)
import           Data.Traversable        (for)
import           Haxl.Core               (GenHaxl)
import           User.Config             (Cache, redisEnv)
import           User.RawAPI             as X (countBindByService,
                                               countBindByUID,
                                               countBindByUIDAndService,
                                               getGroupListByUserId,
                                               getUserIdByName, getUserIdList,
                                               getUserIdListByGroup, mergeData)
import qualified User.RawAPI             as RawAPI
import           User.Types
import           Yuntan.Types.HasMySQL   (HasMySQL, HasOtherEnv)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy, desc)
import           Yuntan.Utils.RedisCache (cached, cached', remove)

($>) :: GenHaxl u a -> GenHaxl u () -> GenHaxl u a
io $> a = do
  !r <- io
  !_ <- a
  return r

genUserKey :: UserID -> ByteString
genUserKey uid = fromString $ "user:" ++ show uid

unCacheUser :: HasOtherEnv Cache u => UserID -> GenHaxl u a -> GenHaxl u a
unCacheUser uid io = io $> remove redisEnv (genUserKey uid)

genCountKey :: String -> ByteString
genCountKey k = fromString $ "count:" ++ k

unCacheCount :: HasOtherEnv Cache u => String -> GenHaxl u a -> GenHaxl u a
unCacheCount k io = io $> remove redisEnv (genCountKey k)

createUser :: (HasMySQL u, HasOtherEnv Cache u) => UserName -> Password -> GenHaxl u UserID
createUser n p = unCacheCount "user" $ RawAPI.createUser n p

getUser :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u (Maybe User)
getUser uid = cached redisEnv (genUserKey uid) $ fillUser =<< RawAPI.getUser uid

getUserByName :: (HasMySQL u, HasOtherEnv Cache u) => UserName -> GenHaxl u (Maybe User)
getUserByName name = maybeM (pure Nothing) getUser $ getUserIdByName name

removeUser :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u Int64
removeUser uid = unCacheUser uid $ unCacheCount "user" $ RawAPI.removeUser uid

updateUserName :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> UserName -> GenHaxl u Int64
updateUserName uid name = unCacheUser uid $ RawAPI.updateUserName uid name

updateUserPassword :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> Password -> GenHaxl u Int64
updateUserPassword uid passwd = unCacheUser uid $ RawAPI.updateUserPassword uid passwd

updateUserExtra :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> Extra -> GenHaxl u Int64
updateUserExtra uid extra = unCacheUser uid $ RawAPI.updateUserExtra uid extra

updateUserSecureExtra :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> Extra -> GenHaxl u Int64
updateUserSecureExtra uid extra = unCacheUser uid $ RawAPI.updateUserSecureExtra uid extra

getUsers :: (HasMySQL u, HasOtherEnv Cache u) => From -> Size -> OrderBy -> GenHaxl u [User]
getUsers from size order = do
  uids <- getUserIdList from size order
  catMaybes <$> for uids getUser

countUser :: (HasMySQL u, HasOtherEnv Cache u) => GenHaxl u Int64
countUser = cached' redisEnv (genCountKey "user") RawAPI.countUser

genBindKey :: BindID -> ByteString
genBindKey bid = fromString $ "bind:" ++ show bid

unCacheBind :: (HasMySQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u a -> GenHaxl u a
unCacheBind bid io = do
  b <- RawAPI.getBind bid
  r <- io
  case b of
    Nothing -> return r
    Just b0 -> do
      remove redisEnv $ genUserKey $ getBindUid b0
      remove redisEnv $ genBindKey $ getBindID b0
      return r

createBind :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> Service -> ServiceName -> Extra -> GenHaxl u BindID
createBind uid se n ex = unCacheUser uid $ RawAPI.createBind uid se n ex

getBind :: (HasMySQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u (Maybe Bind)
getBind bid = cached redisEnv (genBindKey bid) $ RawAPI.getBind bid

getBindByName
  :: (HasMySQL u, HasOtherEnv Cache u) => ServiceName -> GenHaxl u (Maybe Bind)
getBindByName n = maybeM (pure Nothing) getBind $ RawAPI.getBindIdByName n

removeBind :: (HasMySQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u Int64
removeBind bid = unCacheBind bid $ RawAPI.removeBind bid

updateBindExtra :: (HasMySQL u, HasOtherEnv Cache u) => BindID -> Extra -> GenHaxl u Int64
updateBindExtra bid ex = unCacheBind bid $ RawAPI.updateBindExtra bid ex

getBindList :: (HasMySQL u, HasOtherEnv Cache u) => [BindID] -> GenHaxl u [Bind]
getBindList bids = catMaybes <$> for bids getBind

getBindListByUID
  :: (HasMySQL u, HasOtherEnv Cache u)
  => UserID -> From -> Size -> OrderBy -> GenHaxl u [Bind]
getBindListByUID uid f s o = getBindList =<< RawAPI.getBindIdListByUID uid f s o

getBindListByService
  :: (HasMySQL u, HasOtherEnv Cache u)
  => Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
getBindListByService srv f s o =
  getBindList =<< RawAPI.getBindIdListByService srv f s o

getBindListByUIDAndService
  :: (HasMySQL u, HasOtherEnv Cache u)
  => UserID -> Service -> From -> Size -> OrderBy -> GenHaxl u [Bind]
getBindListByUIDAndService uid srv f s o = getBindList =<< RawAPI.getBindIdListByUIDAndService uid srv f s o

removeBindByUID :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u Int64
removeBindByUID uid = unCacheUser uid $ RawAPI.removeBindByUID uid

fillBinds :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillBinds (Just u@User{getUserID = uid}) = do
  binds <- getBindListByUID uid 0 5 $ desc "id"
  return (Just u { getUserBinds = binds })

fillBinds Nothing = return Nothing

groupKey :: GroupName -> String
groupKey name = "group:" ++ T.unpack name

genGroupKey :: GroupName -> ByteString
genGroupKey name = fromString $ groupKey name

unCacheGroup :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u a -> GenHaxl u a
unCacheGroup name io = do
  !r <- io
  remove redisEnv $ genCountKey (groupKey name)
  remove redisEnv $ genGroupKey name
  remove redisEnv $ fromString "groups"
  return r

unCacheGroups :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u a -> GenHaxl u a
unCacheGroups uid io = do
  !r <- io
  names <- getGroupListByUserId uid
  mapM_ (remove redisEnv . genGroupKey) names
  mapM_ (remove redisEnv . genCountKey . groupKey) names
  remove redisEnv $ fromString "groups"
  return r

addGroup :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> UserID -> GenHaxl u ()
addGroup n uid = unCacheUser uid $ unCacheGroup n $ RawAPI.addGroup n uid

removeGroup :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> UserID -> GenHaxl u Int64
removeGroup n uid =
  unCacheUser uid
    $ unCacheGroup n
    $ RawAPI.removeGroup n uid

removeGroupListByUserId :: (HasMySQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u Int64
removeGroupListByUserId uid  =
  unCacheUser uid $ unCacheGroups uid $ RawAPI.removeGroupListByUserId uid

fillGroups :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillGroups (Just u@User{getUserID = uid}) = do
  groups <- getGroupListByUserId uid
  return (Just u { getUserGroups = groups })

fillGroups Nothing = return Nothing

getUserListByGroup
  :: (HasMySQL u, HasOtherEnv Cache u)
  => GroupName -> From -> Size -> OrderBy -> GenHaxl u [User]
getUserListByGroup group f s o = do
  uids <- getUserIdListByGroup group f s o
  catMaybes <$> for uids getUser

countGroup :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u Int64
countGroup name =
  cached' redisEnv (genCountKey (groupKey name)) $ RawAPI.countGroup name

fillUser :: (HasMySQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u (Maybe User)
fillUser u = fillGroups =<< fillBinds u

saveGroupMeta :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> GroupTitle -> GroupSummary -> GenHaxl u Int64
saveGroupMeta n t s = unCacheGroup n $ RawAPI.saveGroupMeta n t s

removeGroupMeta :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u Int64
removeGroupMeta n = unCacheGroup n $ RawAPI.removeGroupMeta n

getGroupMeta :: (HasMySQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u (Maybe GroupMeta)
getGroupMeta n = cached redisEnv (genGroupKey n) $ fillGroupUserCount =<< RawAPI.getGroupMeta n

getGroupMetaList :: (HasMySQL u, HasOtherEnv Cache u) => GenHaxl u [GroupMeta]
getGroupMetaList = cached' redisEnv (fromString "groups") $ do
  gs <- RawAPI.getGroupMetaList
  catMaybes <$> for gs (fillGroupUserCount . Just)

fillGroupUserCount :: HasMySQL u => Maybe GroupMeta -> GenHaxl u (Maybe GroupMeta)
fillGroupUserCount (Just g@GroupMeta{getGroup = group}) = do
  uc <- RawAPI.countGroup group
  return (Just g {getGroupUserCount = uc})

fillGroupUserCount Nothing = return Nothing
