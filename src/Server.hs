{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( runTwitterServer
    ) where

import           RIO

import           Control.Exception.Safe      as C (Handler (..), catches)
import           Control.Monad.Logger        (runStderrLoggingT)
import qualified RIO.ByteString.Lazy         as LBS
import qualified RIO.Text                    as T

import           Data.Aeson                  (ToJSON)
import           Database.Persist.Postgresql (ConnectionPool,
                                              createPostgresqlPool,
                                              runMigration, runSqlPool,
                                              toSqlKey)

import           Network.Wai.Handler.Warp    as Warp

import           Say                         (say)
import           Servant                     as S

import           Api                         (Api, api)
import           Configuration               (Config (..), setupConfig)
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
getTweetsByUserH userName = withConnPool $ \pool -> getTweetsByUser pool userName

-- | Get user profile
getUserProfileH :: UserName -> AppM User
getUserProfileH userName = withConnPool $ \pool -> getUserByName pool userName

-- | Create user with given UserName
createUserH :: UserName -> AppM User
createUserH userName = do
    cfg <- ask
    handleWithException $ withConnPool $ \pool -> insertUser pool cfg userName

-- | Get Tweet by its Id
getTweetByIdH :: Int64 -> AppM (Sorted Tweet)
getTweetByIdH tweetNum = do
    let tweetId = toSqlKey tweetNum
    handleWithException $ withConnPool $ \pool -> getTweetById pool tweetId

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
    pool <- runStderrLoggingT $ createPostgresqlPool (cfgConnectionString config) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app config

-- | Run given action with connection pool
withConnPool :: (ToJSON a) => (ConnectionPool -> IO a)-> AppM a
withConnPool action = do
    config <- ask
    pool   <- liftIO $ runStderrLoggingT $ createPostgresqlPool (cfgConnectionString config) 5
    liftIO $ action pool

-- | Run twitter server
runTwitterServer :: IO ()
runTwitterServer = do

    config <- setupConfig

    say $ "Starting " <> cfgServerName config <> " on port " <> tshow (cfgPortNumber config)

    application <- mkApp config
    Warp.run (cfgPortNumber config) application
