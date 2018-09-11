{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator
    ( tweetRandomly
    , insertUsers
    , insertRandomDataIntoEmptyDB
    ) where

import           RIO

import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.String.Conversions (cs)
import           Database.Persist.Sqlite
import           Say                     (say)
import           Test.QuickCheck         (Gen, arbitrary, elements, generate,
                                          vectorOf)

import           Lib                     (getLatestTweetId, insertTweet,
                                          insertUser)

import           Configuration           (Config (..))
import           Exceptions              (TwitterException (..))
import           Model                   (Tweet (..), UserName,
                                          testUserList)

--------------------------------------------------------------------------------
-- Random generator to facilitate data insertion
--------------------------------------------------------------------------------

-- | Tweet type
data TweetType =  Normal | Response
    deriving (Show)

-- | Insert random tweets into the database
tweetRandomly :: FilePath -> Int -> IO ()
tweetRandomly sqliteFile num = do
     randomList <- generate $ vectorOf num (elements [Normal, Response])
     pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5

     forM_ randomList $ \case
        Normal   -> insertRandomTweet pool
        Response -> replyRandomTweet pool

-- | Insert random tweet
insertRandomTweet :: ConnectionPool -> IO ()
insertRandomTweet pool = do
    randomTweet <- generate mkRandomTweet
    let userName = tAuthor randomTweet
        content  = tContent randomTweet
    ignoreException $ void $ insertTweet pool userName content Nothing

-- | Reply to random tweet
replyRandomTweet :: ConnectionPool -> IO ()
replyRandomTweet pool = do
    latestTweetId <- getLatestTweetId pool
    case latestTweetId of
        Nothing   -> insertRandomTweet pool
        (Just num) -> do
            randomTweet <- generate mkRandomTweet
            let userName = tAuthor randomTweet
                content  = tContent randomTweet
            randomId <- generate $ elements [1 .. num]
            ignoreException $ void $ insertTweet pool userName content (Just randomId)

-- | Generate random tweet with no replies and parentId
mkRandomTweet :: Gen Tweet
mkRandomTweet = do
    randomTweet <- arbitrary :: Gen Tweet
    return $ randomTweet { tReplyTo = Nothing, tReplies = []}

-- | Insert Users into given databse
insertUsers :: FilePath -> [UserName] -> IO ()
insertUsers sqliteFile users = do
    pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5

    forM_ users $ \user ->
        ignoreException $ void $ insertUser pool user

-- | Exception handling for generator
ignoreException :: IO () -> IO ()
ignoreException = handle handleException
  where
    handleException :: TwitterException -> IO ()
    handleException e = do
        say $ tshow e
        return ()

insertRandomDataIntoEmptyDB :: Config -> IO ()
insertRandomDataIntoEmptyDB cfg = do
    insertUsers (cfgDevelopmentDBPath cfg) testUserList
    tweetRandomly (cfgDevelopmentDBPath cfg) 100

