{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)
import           Yuntan.Types.HasMySQL                (HasMySQL, simpleEnv)
import           Yuntan.Types.Scotty                  (ScottyH)

import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)
import           User
import           User.APIHandler

import qualified Data.Yaml                            as Y
import qualified User.Config                          as C

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "User micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "User micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "User micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "User micro server"
     <> header "yuntan-user - User micro server" )

program :: Options -> IO ()
program Options { getConfigFile  = confFile
                , getTablePrefix = prefix
                , getHost        = host
                , getPort        = port
                } = do
  (Just conf) <- Y.decodeFile confFile :: IO (Maybe C.Config)

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initUserState mysqlThreads) stateEmpty

  let u = simpleEnv pool prefix

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  runIO u state mergeData
  scottyOptsT opts (runIO u state) application
  where
        runIO :: HasMySQL u => u -> StateStore -> GenHaxl u b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: HasMySQL u => ScottyH u ()
application = do
  middleware logStdout

  get    "/api/users/"                       getUsersAPIHandler
  post   "/api/users/"                       createUserAPIHandler
  get    "/api/users/:uidOrName/"            $ requireUser getUserAPIHandler
  delete "/api/users/:uidOrName/"            $ requireUser removeUserAPIHandler

  post   "/api/users/:uidOrName/"            $ requireUser updateUserNameAPIHandler
  post   "/api/users/:uidOrName/passwd"      $ requireUser updateUserPasswordAPIHandler
  post   "/api/users/:uidOrName/extra"       $ requireUser updateUserExtraAPIHandler
  delete "/api/users/:uidOrName/extra"       $ requireUser removeUserExtraAPIHandler
  post   "/api/users/:uidOrName/extra/clear" $ requireUser clearUserExtraAPIHandler
  post   "/api/users/:uidOrName/verify"      $ requireUser verifyPasswordAPIHandler

  post   "/api/users/:uidOrName/binds/"      $ requireUser createBindAPIHandler

  get    "/api/users/:uidOrName/binds/"      $ requireUser getBindListByUserAPIHandler
  get    "/api/users/:uidOrName/binds/:service" $ requireUser getBindListByUserAndServiceAPIHandler

  post   "/api/groups/:group/:uidOrName/"    $ requireUser createGroupAPIHandler
  delete "/api/groups/:group/:uidOrName/"    $ requireUser removeGroupAPIHandler
  get    "/api/groups/:group/"               getUserListByGroupAPIHandler

  get    "/api/binds/"                       getBindAPIHandler
  delete "/api/binds/:bind_id"               removeBindAPIHandler
  get    "/api/service/:service/binds/"      getBindListByServiceAPIHandler

  get    "/api/graphql/" graphqlHandler
  post   "/api/graphql/" graphqlHandler

  get    "/api/binds/:name/graphql/"  graphqlByBindHandler
  post   "/api/binds/:name/graphql/" graphqlByBindHandler

  get    "/api/users/:uidOrName/graphql/" $ requireUser graphqlByUserHandler
  post   "/api/users/:uidOrName/graphql/" $ requireUser graphqlByUserHandler
