{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( Server.runServer
    ) where

import           RIO

import           Control.Exception.Safe   as C (try)
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
import           Lib                      (getAllTweets, getTweetById,
                                           getTweetsByUser, getUserByName,
                                           insertUser)
import           Model                    (Tweet (..), User (..), UserName,
                                           migrateAll, Validate(..))

-- | Server endpoints
server :: ConnectionPool -> Config -> Server Api
server pool config = getAllTweetsH pool
    :<|> getTweetsByUserH pool
    :<|> getUserProfileH pool
    :<|> createUserH pool config
    :<|> getTweetByIdH pool

--------------------------------------------------------------------------------
-- Endpoint handling
--------------------------------------------------------------------------------

-- | Return all the tweets from database
getAllTweetsH :: ConnectionPool -> S.Handler [Tweet]
getAllTweetsH pool = liftIO $ getAllTweets pool

-- | Get all the tweets from user
getTweetsByUserH :: ConnectionPool -> UserName -> S.Handler [Tweet]
getTweetsByUserH pool userName = liftIO $ getTweetsByUser pool userName

-- | Get user profile
getUserProfileH :: ConnectionPool -> UserName -> S.Handler User
getUserProfileH pool userName = liftIO $ getUserByName pool userName

-- | Create user with given UserName
createUserH :: ConnectionPool -> Config -> UserName -> S.Handler User
createUserH pool cfg userName =
    case validate cfg userName of
        Left e -> throwError err400 {errBody = showError e}
        Right validUserName -> do
            eResult <- liftIO $ C.try $ insertUser pool validUserName
            handleTwitterException eResult

-- | Get Tweet by its Id
getTweetByIdH :: ConnectionPool -> Int64 -> S.Handler Tweet
getTweetByIdH pool tweetNum = do
    let tweetId = toSqlKey tweetNum
    eResult <- liftIO $ C.try $ getTweetById pool tweetId
    handleTwitterException eResult

-- | Show error in Lazy Bytestring
showError :: (IsString a, Exception e) => e -> a
showError =  fromString . show

-- | Exception handling on Twitter error
handleTwitterException :: (ToJSON a) => Either TwitterException a -> S.Handler a
handleTwitterException (Left e)  = throwError err400 {errBody = showError e}
handleTwitterException (Right a) = return a

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
runServer :: IO ()
runServer = do
    let config = defaultConfig
    say $ "Starting " <> cfgServerName config <> " on port " <> tshow (cfgPortNumber config)
    application <- mkApp config
    Warp.run (cfgPortNumber config) application
