{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( runTwitterServer
    ) where

import           RIO

import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
import           Control.Exception.Safe   as C (Handler (..), catches)
import           Control.Monad.Logger     (runStderrLoggingT)

import           Data.Aeson               (ToJSON)
import           Data.String.Conversions  (cs)
import           Database.Persist.Sqlite  (ConnectionPool, createSqlitePool,
                                           runMigration, runSqlPool, toSqlKey)

import           Network.Wai.Handler.Warp as Warp

import           Say                      (say)
import           Servant                  as S

import           Api                      (Api, api)
import           Configuration            (Config (..), defaultConfig)
import           Exceptions               (TwitterException (..))
import           Lib                      (Sorted, getTweetById,
                                           getTweetsByUser, getUserByName,
                                           insertUser)
import           Model                    (Tweet (..), User (..), UserName,
                                           ValidationException (..), migrateAll)

-- | Server endpoints
server :: ConnectionPool -> ServerT Api AppM
server pool =
         getTweetsByUserH pool
    :<|> getUserProfileH pool
    :<|> createUserH pool
    :<|> getTweetByIdH pool

--------------------------------------------------------------------------------
-- Endpoint handling
--------------------------------------------------------------------------------

-- | Get all the tweets from user
getTweetsByUserH :: ConnectionPool -> UserName -> AppM (Sorted [Tweet])
getTweetsByUserH pool userName = liftIO $ getTweetsByUser pool userName

-- | Get user profile
getUserProfileH :: ConnectionPool -> UserName -> AppM User
getUserProfileH pool userName = liftIO $ getUserByName pool userName

-- | Create user with given UserName
createUserH :: ConnectionPool -> UserName -> AppM User
createUserH pool userName = do
    cfg <- ask
    handleWithException $ insertUser pool cfg userName


-- | Get Tweet by its Id
getTweetByIdH :: ConnectionPool -> Int64 -> AppM (Sorted Tweet)
getTweetByIdH pool tweetNum = do
    let tweetId = toSqlKey tweetNum
    handleWithException $ getTweetById pool tweetId

-- | Exception handling
handleWithException :: (ToJSON a) => IO a -> AppM a
handleWithException action = C.catches (liftIO action)
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
app :: ConnectionPool -> Config -> Application
app pool config = serve api $ hoistServer api (nt config) (server pool)

-- | Run application with given file as database
mkApp :: Config -> IO Application
mkApp config = do
    pool <- runStderrLoggingT $ createSqlitePool (cs $ cfgDevelopmentDBPath config) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool config

-- | Run twitter server
runTwitterServer :: IO ()
runTwitterServer = do
    let config = defaultConfig
    say $ "Starting " <> cfgServerName config <> " on port " <> tshow (cfgPortNumber config)
    application <- mkApp config
    Warp.run (cfgPortNumber config) application
