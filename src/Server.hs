{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Server
    ( runTwitterServer
    , runAction
    ) where

import           RIO

import           Control.Exception.Safe      as C (Handler (..), catches)
import           Control.Monad.Logger        (runNoLoggingT)
import qualified RIO.ByteString.Lazy         as LBS
import qualified RIO.Text                    as T

import           Data.Aeson                  (ToJSON)
import           Database.Persist.Postgresql (createPostgresqlPool,
                                              runMigration, runSqlPool,
                                              toSqlKey)

import           Network.Wai.Handler.Warp    as Warp

import           Say                         (say)
import           Servant                     as S

import           Api                         (Api, api)
import           Configuration               (Config (..), Env (..),
                                              defaultConfig)
import           Exceptions                  (TwitterException (..))
import           Lib                         (Sorted, getTweetById,
                                              getTweetsByUser, getUserByName,
                                              insertUser)
import           Model                       (Tweet (..), User (..), UserName,
                                              ValidationException (..),
                                              migrateAll)

-- | Server endpoints
server :: ServerT Api AppM
server = getTweetsByUserH
    :<|> getUserProfileH
    :<|> createUserH
    :<|> getTweetByIdH

--------------------------------------------------------------------------------
-- Endpoint handling
--------------------------------------------------------------------------------

-- | Get all the tweets from user
getTweetsByUserH :: UserName -> AppM (Sorted [Tweet])
getTweetsByUserH userName = runAction $ getTweetsByUser userName

-- | Get user profile
getUserProfileH :: UserName -> AppM User
getUserProfileH userName = runAction $ getUserByName userName

-- | Create user with given UserName
createUserH :: UserName -> AppM User
createUserH userName = do
    cfg <- ask
    handleWithException $ runAction $ insertUser cfg userName

-- | Get Tweet by its Id
getTweetByIdH :: Int64 -> AppM (Sorted Tweet)
getTweetByIdH tweetNum = do
    let tweetId = toSqlKey tweetNum
    handleWithException $ runAction $ getTweetById tweetId

-- | Exception handling
handleWithException :: (ToJSON a) => AppM a -> AppM a
handleWithException action = C.catches action
     [C.Handler validationHandler, C.Handler twitterHandler]
  where
    validationHandler :: ValidationException -> AppM a
    validationHandler e = throwError err400 {errBody = showError e}
    twitterHandler :: TwitterException -> AppM a
    twitterHandler e = throwError err400 {errBody = showError e}
    showError :: (Exception e) => e -> LBS.ByteString
    showError =  LBS.fromStrict . encodeUtf8 . T.pack . show

--------------------------------------------------------------------------------
-- Server logic
--------------------------------------------------------------------------------

type AppM = ReaderT Config S.Handler

nt :: Config -> AppM a -> S.Handler a
nt c x = runReaderT x c

-- (TODO): Use proper SQL?
app :: Config -> Application
app config = serve api $ hoistServer api (nt config) server

-- | Run application with given file as database
mkApp :: Config -> IO Application
mkApp config = do
    pool <- runNoLoggingT $ createPostgresqlPool (cfgConnectionString config) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app config

-- | Run given action with connection pool
runAction :: (ToJSON a) => RIO Env a -> AppM a
runAction action = do
    envConfig <- ask -- This is actually confusing
    envPool   <- liftIO $ runNoLoggingT $ createPostgresqlPool (cfgConnectionString envConfig) 5
    logOpts   <- logOptionsHandle stdout False
    liftIO $ withLogFunc logOpts $ \envLogFunc ->
        runRIO Env{..} action

-- | Run twitter server
runTwitterServer :: IO ()
runTwitterServer = do

    config <- defaultConfig

    say $ "Starting " <> cfgServerName config <> " on port " <> tshow (cfgPortNumber config)

    application <- mkApp config
    Warp.run (cfgPortNumber config) application
