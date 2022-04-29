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

import           Control.Monad.Extra (maybeM)
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)
import           Data.Maybe          (catMaybes)
import           Data.String         (fromString)
import qualified Data.Text           as T (unpack)
import           Data.Traversable    (for)
import           Database.PSQL.Types (From, HasOtherEnv, HasPSQL, OrderBy, Size,
                                      desc)
import           Haxl.Core           (GenHaxl)
import           Haxl.RedisCache     (cached, cached', remove)
import           User.Config         (Cache, redisEnv)
import qualified User.RawAPI         as RawAPI
import           User.RawAPI         as X (countBindByService, countBindByUID,
                                           countBindByUIDAndService,
                                           getGroupListByUserId,
                                           getUserIdByName, getUserIdList,
                                           getUserIdListByGroup, mergeData)
import           User.Types

($>) :: GenHaxl u w a -> GenHaxl u w () -> GenHaxl u w a
io $> a = do
  !r <- io
  !_ <- a
  return r

genUserKey :: UserID -> ByteString
genUserKey uid = fromString $ "user:" ++ show uid

unCacheUser :: HasOtherEnv Cache u => UserID -> GenHaxl u w a -> GenHaxl u w a
unCacheUser uid io = io $> remove redisEnv (genUserKey uid)

genCountKey :: String -> ByteString
genCountKey k = fromString $ "count:" ++ k

unCacheCount :: HasOtherEnv Cache u => String -> GenHaxl u w a -> GenHaxl u w a
unCacheCount k io = io $> remove redisEnv (genCountKey k)

createUser :: (HasPSQL u, HasOtherEnv Cache u) => UserName -> Password -> GenHaxl u w UserID
createUser n p = unCacheCount "user" $ RawAPI.createUser n p

getUser :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u w (Maybe User)
getUser uid = cached redisEnv (genUserKey uid) $ fillUser =<< RawAPI.getUser uid

getUserByName :: (HasPSQL u, HasOtherEnv Cache u) => UserName -> GenHaxl u w (Maybe User)
getUserByName name = maybeM (pure Nothing) getUser $ getUserIdByName name

removeUser :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u w Int64
removeUser uid = unCacheUser uid $ unCacheCount "user" $ RawAPI.removeUser uid

updateUserName :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> UserName -> GenHaxl u w Int64
updateUserName uid name = unCacheUser uid $ RawAPI.updateUserName uid name

updateUserPassword :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> Password -> GenHaxl u w Int64
updateUserPassword uid passwd = unCacheUser uid $ RawAPI.updateUserPassword uid passwd

updateUserExtra :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> Extra -> GenHaxl u w Int64
updateUserExtra uid extra = unCacheUser uid $ RawAPI.updateUserExtra uid extra

updateUserSecureExtra :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> Extra -> GenHaxl u w Int64
updateUserSecureExtra uid extra = unCacheUser uid $ RawAPI.updateUserSecureExtra uid extra

getUsers :: (HasPSQL u, HasOtherEnv Cache u) => From -> Size -> OrderBy -> GenHaxl u w [User]
getUsers from size order = do
  uids <- getUserIdList from size order
  catMaybes <$> for uids getUser

countUser :: (HasPSQL u, HasOtherEnv Cache u) => GenHaxl u w Int64
countUser = cached' redisEnv (genCountKey "user") RawAPI.countUser

genBindKey :: BindID -> ByteString
genBindKey bid = fromString $ "bind:" ++ show bid

unCacheBind :: (HasPSQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u w a -> GenHaxl u w a
unCacheBind bid io = do
  b <- RawAPI.getBind bid
  r <- io
  case b of
    Nothing -> return r
    Just b0 -> do
      remove redisEnv $ genUserKey $ getBindUid b0
      remove redisEnv $ genBindKey $ getBindID b0
      return r

createBind :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> Service -> ServiceName -> Extra -> GenHaxl u w BindID
createBind uid se n ex = unCacheUser uid $ RawAPI.createBind uid se n ex

getBind :: (HasPSQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u w (Maybe Bind)
getBind bid = cached redisEnv (genBindKey bid) $ RawAPI.getBind bid

getBindByName
  :: (HasPSQL u, HasOtherEnv Cache u) => ServiceName -> GenHaxl u w (Maybe Bind)
getBindByName n = maybeM (pure Nothing) getBind $ RawAPI.getBindIdByName n

removeBind :: (HasPSQL u, HasOtherEnv Cache u) => BindID -> GenHaxl u w Int64
removeBind bid = unCacheBind bid $ RawAPI.removeBind bid

updateBindExtra :: (HasPSQL u, HasOtherEnv Cache u) => BindID -> Extra -> GenHaxl u w Int64
updateBindExtra bid ex = unCacheBind bid $ RawAPI.updateBindExtra bid ex

getBindList :: (HasPSQL u, HasOtherEnv Cache u) => [BindID] -> GenHaxl u w [Bind]
getBindList bids = catMaybes <$> for bids getBind

getBindListByUID
  :: (HasPSQL u, HasOtherEnv Cache u)
  => UserID -> From -> Size -> OrderBy -> GenHaxl u w [Bind]
getBindListByUID uid f s o = getBindList =<< RawAPI.getBindIdListByUID uid f s o

getBindListByService
  :: (HasPSQL u, HasOtherEnv Cache u)
  => Service -> From -> Size -> OrderBy -> GenHaxl u w [Bind]
getBindListByService srv f s o =
  getBindList =<< RawAPI.getBindIdListByService srv f s o

getBindListByUIDAndService
  :: (HasPSQL u, HasOtherEnv Cache u)
  => UserID -> Service -> From -> Size -> OrderBy -> GenHaxl u w [Bind]
getBindListByUIDAndService uid srv f s o = getBindList =<< RawAPI.getBindIdListByUIDAndService uid srv f s o

removeBindByUID :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u w Int64
removeBindByUID uid = unCacheUser uid $ RawAPI.removeBindByUID uid

fillBinds :: (HasPSQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u w (Maybe User)
fillBinds (Just u@User{getUserID = uid}) = do
  binds <- getBindListByUID uid 0 5 $ desc "id"
  return (Just u { getUserBinds = binds })

fillBinds Nothing = return Nothing

groupKey :: GroupName -> String
groupKey name = "group:" ++ T.unpack name

genGroupKey :: GroupName -> ByteString
genGroupKey name = fromString $ groupKey name

unCacheGroup :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u w a -> GenHaxl u w a
unCacheGroup name io = do
  !r <- io
  remove redisEnv $ genCountKey (groupKey name)
  remove redisEnv $ genGroupKey name
  remove redisEnv $ fromString "groups"
  return r

unCacheGroups :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u w a -> GenHaxl u w a
unCacheGroups uid io = do
  !r <- io
  names <- getGroupListByUserId uid
  mapM_ (remove redisEnv . genGroupKey) names
  mapM_ (remove redisEnv . genCountKey . groupKey) names
  remove redisEnv $ fromString "groups"
  return r

addGroup :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> UserID -> GenHaxl u w Int64
addGroup n uid = unCacheUser uid $ unCacheGroup n $ RawAPI.addGroup n uid

removeGroup :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> UserID -> GenHaxl u w Int64
removeGroup n uid =
  unCacheUser uid
    $ unCacheGroup n
    $ RawAPI.removeGroup n uid

removeGroupListByUserId :: (HasPSQL u, HasOtherEnv Cache u) => UserID -> GenHaxl u w Int64
removeGroupListByUserId uid  =
  unCacheUser uid $ unCacheGroups uid $ RawAPI.removeGroupListByUserId uid

fillGroups :: (HasPSQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u w (Maybe User)
fillGroups (Just u@User{getUserID = uid}) = do
  groups <- getGroupListByUserId uid
  return (Just u { getUserGroups = groups })

fillGroups Nothing = return Nothing

getUserListByGroup
  :: (HasPSQL u, HasOtherEnv Cache u)
  => GroupName -> From -> Size -> OrderBy -> GenHaxl u w [User]
getUserListByGroup group f s o = do
  uids <- getUserIdListByGroup group f s o
  catMaybes <$> for uids getUser

countGroup :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u w Int64
countGroup name =
  cached' redisEnv (genCountKey (groupKey name)) $ RawAPI.countGroup name

fillUser :: (HasPSQL u, HasOtherEnv Cache u) => Maybe User -> GenHaxl u w (Maybe User)
fillUser u = fillGroups =<< fillBinds u

saveGroupMeta :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> GroupTitle -> GroupSummary -> GenHaxl u w Int64
saveGroupMeta n t s = unCacheGroup n $ RawAPI.saveGroupMeta n t s

removeGroupMeta :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u w Int64
removeGroupMeta n = unCacheGroup n $ RawAPI.removeGroupMeta n

getGroupMeta :: (HasPSQL u, HasOtherEnv Cache u) => GroupName -> GenHaxl u w (Maybe GroupMeta)
getGroupMeta n = cached redisEnv (genGroupKey n) $ fillGroupUserCount =<< RawAPI.getGroupMeta n

getGroupMetaList :: (HasPSQL u, HasOtherEnv Cache u) => GenHaxl u w [GroupMeta]
getGroupMetaList = cached' redisEnv (fromString "groups") $ do
  gs <- RawAPI.getGroupMetaList
  catMaybes <$> for gs (fillGroupUserCount . Just)

fillGroupUserCount :: HasPSQL u => Maybe GroupMeta -> GenHaxl u w (Maybe GroupMeta)
fillGroupUserCount (Just g@GroupMeta{getGroup = group}) = do
  uc <- RawAPI.countGroup group
  return (Just g {getGroupUserCount = uc})

fillGroupUserCount Nothing = return Nothing
