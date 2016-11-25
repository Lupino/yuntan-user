{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Pool                            (createPool)
import           Database.MySQL.Simple                (ConnectInfo (..), close,
                                                       connect,
                                                       defaultConnectInfo)

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Control.Monad                        (when)
import           Dispatch
import           Dispatch.APIHandler
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)

import           Data.Text                            (pack)
import           Data.Yaml.Config                     as Y (load, lookupDefault,
                                                            subconfig)
import           Options.Applicative

data Options = Options { getConfigFile :: String }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Dispatch config file."
                                <> value "config.yaml")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "User micro server"
     <> header "dispatch-user - User micro server" )

program :: Options -> IO ()
program opts = do
  yamlConfig <- load $ getConfigFile opts
  serverConfig <- subconfig "server" yamlConfig
  mysqlConfig <- subconfig "mysql" yamlConfig
  let serverHost   = Y.lookupDefault "host" "127.0.0.1" serverConfig
      serverPort   = Y.lookupDefault "port" 3000 serverConfig
      dbName       = Y.lookupDefault "db" "dispatch" mysqlConfig
      dbHost       = Y.lookupDefault "host" "127.0.0.1" mysqlConfig
      dbPort       = Y.lookupDefault "port" 3306 mysqlConfig
      dbUser       = Y.lookupDefault "user" "root" mysqlConfig
      dbPass       = Y.lookupDefault "pass" "" mysqlConfig
      numStripes   = Y.lookupDefault "numStripes" 1 mysqlConfig
      idleTime     = Y.lookupDefault "idleTime" 0.5 mysqlConfig
      maxResources = Y.lookupDefault "maxResources" 1 mysqlConfig
      numThreads   = Y.lookupDefault "numThreads" 1 mysqlConfig
      tablePrefix  = Y.lookupDefault "tablePrefix" "test" mysqlConfig

  let conn = connect defaultConnectInfo { connectDatabase = dbName
                                        , connectHost = dbHost
                                        , connectPort = dbPort
                                        , connectUser = dbUser
                                        , connectPassword = dbPass
                                        }

  pool <- createPool conn close numStripes idleTime maxResources

  let state = initGlobalState numThreads

  let config = UserEnv { mySQLPool = pool, tablePrefix = tablePrefix }

  let opts = def { settings = setPort serverPort
                            $ setHost (Host serverHost) (settings def) }

  scottyOptsT opts (runIO config state) application
  where
        runIO :: UserEnv -> StateStore -> DispatchM b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: ScottyM ()
application = do
  middleware logStdout

  get    "/api/users/"                       getUsersAPIHandler
  post   "/api/users/"                       createUserAPIHandler
  get    "/api/users/:uidOrName/"            getUserAPIHandler
  delete "/api/users/:uidOrName/"            $ requireUser removeUserAPIHandler

  post   "/api/users/:uidOrName/"            $ requireUser updateUserNameAPIHandler
  post   "/api/users/:uidOrName/password"    $ requireUser updateUserPasswordAPIHandler
  post   "/api/users/:uidOrName/extra"       $ requireUser updateUserExtraAPIHandler
  delete "/api/users/:uidOrName/extra"       $ requireUser removeUserExtraAPIHandler
  post   "/api/users/:uidOrName/extra/clear" $ requireUser clearUserExtraAPIHandler

  post   "/api/users/:uidOrName/binds"       $ requireUser createBindAPIHandler

  get    "/api/binds/"                       getBindAPIHandler
  delete "/api/binds/:bind_id"               removeBindAPIHandler
