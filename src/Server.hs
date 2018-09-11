{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Configuration            (Config(..), defaultConfig)

import           Exceptions               (TwitterException (..))
import           Lib                      (getAllTweets, getTweetById,
                                           getTweetsByUser, insertUser)
import           Model                    (DBUser (..), Tweet (..),
                                           UserName (..), migrateAll)

-- | Server endpoints
server :: ConnectionPool -> Server Api
server pool = getAllTweetsH pool
    :<|> getTweetsByUserH pool
    :<|> createUserH pool
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

-- | Create user with given UserName
createUserH :: ConnectionPool -> UserName -> S.Handler DBUser
createUserH pool userName = do
    eResult <- liftIO $ C.try $ insertUser pool userName
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
app :: ConnectionPool -> Application
app pool = serve api $ server pool

-- | Run application with given file as database
mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
    pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5

    runSqlPool (runMigration migrateAll) pool
    return $ app pool

-- | Run application with given file as database
runServer :: IO ()
runServer = do
    let Config{..} = defaultConfig
    say $ "Starting " <> cfgServerName <> " on port " <> tshow cfgPortNumber
    application <- mkApp cfgDevelopmentDBPath
    Warp.run cfgPortNumber application
