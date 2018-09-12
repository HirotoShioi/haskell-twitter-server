{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( runTwitterServer
    ) where

import           RIO

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
import           Lib                      (getTweetById, getTweetsByUser,
                                           getUserByName, insertUser)
import           Model                    (Tweet (..), User (..), UserName,
                                           ValidationException (..), migrateAll)

-- | Server endpoints
server :: ConnectionPool -> Config -> Server Api
server pool config =
         getTweetsByUserH pool
    :<|> getUserProfileH pool
    :<|> createUserH pool config
    :<|> getTweetByIdH pool

--------------------------------------------------------------------------------
-- Endpoint handling
--------------------------------------------------------------------------------

-- | Get all the tweets from user
getTweetsByUserH :: ConnectionPool -> UserName -> S.Handler [Tweet]
getTweetsByUserH pool userName = liftIO $ getTweetsByUser pool userName

-- | Get user profile
getUserProfileH :: ConnectionPool -> UserName -> S.Handler User
getUserProfileH pool userName = liftIO $ getUserByName pool userName

-- | Create user with given UserName
createUserH :: ConnectionPool -> Config -> UserName -> S.Handler User
createUserH pool cfg userName = handleWithException $ insertUser pool cfg userName


-- | Get Tweet by its Id
getTweetByIdH :: ConnectionPool -> Int64 -> S.Handler Tweet
getTweetByIdH pool tweetNum = do
    let tweetId = toSqlKey tweetNum
    handleWithException $ getTweetById pool tweetId

-- | Exception handling
handleWithException :: (ToJSON a) => IO a -> S.Handler a
handleWithException action = C.catches (liftIO action)
     [C.Handler validationHandler, C.Handler twitterHandler]
  where
    validationHandler :: ValidationException -> S.Handler a
    validationHandler e = throwError err400 {errBody = showError e}
    twitterHandler :: TwitterException -> S.Handler a
    twitterHandler e = throwError err400 {errBody = showError e}
    showError :: (IsString a, Exception e) => e -> a
    showError =  fromString . show


--------------------------------------------------------------------------------
-- Server logic
--------------------------------------------------------------------------------

-- (TODO): Use proper SQL?
app :: ConnectionPool -> Config -> Application
app pool config = serve api $ server pool config

-- | Run application with given file as database
mkApp :: Config -> IO Application
mkApp config = do
    pool <- runStderrLoggingT $ createSqlitePool (cs $ cfgDevelopmentDBPath config) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool config

-- | Run application with given file as database
runTwitterServer :: IO ()
runTwitterServer = do
    let config = defaultConfig
    say $ "Starting " <> cfgServerName config <> " on port " <> tshow (cfgPortNumber config)
    application <- mkApp config
    Warp.run (cfgPortNumber config) application
