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
import           User.Handler

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

  get    "/api/users/"                       getUsersHandler
  post   "/api/users/"                       createUserHandler
  get    "/api/users/:uidOrName/"            $ requireUser getUserHandler
  delete "/api/users/:uidOrName/"            $ requireUser removeUserHandler

  post   "/api/users/:uidOrName/"            $ requireUser updateUserNameHandler
  post   "/api/users/:uidOrName/passwd"      $ requireUser updateUserPasswordHandler
  post   "/api/users/:uidOrName/extra"       $ requireUser updateUserExtraHandler
  delete "/api/users/:uidOrName/extra"       $ requireUser removeUserExtraHandler
  post   "/api/users/:uidOrName/extra/clear" $ requireUser clearUserExtraHandler
  post   "/api/users/:uidOrName/verify"      $ requireUser verifyPasswordHandler

  post   "/api/users/:uidOrName/binds/"      $ requireUser createBindHandler

  get    "/api/users/:uidOrName/binds/"      $ requireUser getBindListByUserHandler
  get    "/api/users/:uidOrName/binds/:service/" $ requireUser getBindListByUserAndServiceHandler

  post   "/api/groups/:group/:uidOrName/"    $ requireUser createGroupHandler
  delete "/api/groups/:group/:uidOrName/"    $ requireUser removeGroupHandler
  get    "/api/groups/:group/"               getUserListByGroupHandler

  get    "/api/binds/"                       getBindHandler
  delete "/api/binds/:bind_id/"              removeBindHandler
  post   "/api/binds/:bind_id/"              updateBindExtraHandler
  get    "/api/service/:service/binds/"      getBindListByServiceHandler

  post   "/api/graphql/" graphqlHandler
  post   "/api/binds/:name/graphql/" graphqlByBindHandler
  post   "/api/users/:uidOrName/graphql/" $ requireUser graphqlByUserHandler
  post   "/api/service/:service/graphql/" graphqlByServiceHandler
