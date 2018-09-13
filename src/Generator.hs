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

import           Configuration           (Config (..))
import           Exceptions              (TwitterException (..))
import           Lib                     (getLatestTweetId, getTweetById,
                                          insertTweet, insertUser)
import           Model                   (Tweet (..), TweetId (..),
                                          TweetText (..), UserName (..),
                                          migrateAll, testUserList)

--------------------------------------------------------------------------------
-- Random generator to facilitate data insertion
--------------------------------------------------------------------------------
-- FIX GENERATOR

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
        content  = tText randomTweet
    ignoreException $ void $ insertTweet pool userName content Nothing []

-- | Reply to random tweet
-- Write better reply (add @Mention)
replyRandomTweet :: ConnectionPool -> IO ()
replyRandomTweet pool = do
    -- Get latest tweet Id
    latestTweetId <- getLatestTweetId pool
    case latestTweetId of
        Nothing   -> insertRandomTweet pool
        (Just num) -> do
            -- Get random Id
            randomId <- generate $ elements [1 .. (getTweetId num)]
            
            -- Fetch an tweet with given random Id
            randomlyFetchedTweet <- getTweetById pool (toSqlKey randomId)

            -- Generate an reply to an tweet
            randomReply <- generate mkRandomTweet
            let parentAuthor = getUserName $ tAuthor randomlyFetchedTweet
            let postUser = tAuthor randomReply
            let content = "@" <> parentAuthor <> " " <> getTweetText (tText randomReply)

            -- Insert an reply
            ignoreException $
                void $ insertTweet pool postUser (TweetText content) (Just $ TweetId randomId) []

-- | Generate random tweet with no replies and parentId
mkRandomTweet :: Gen Tweet
mkRandomTweet = do
    randomTweet <- arbitrary :: Gen Tweet
    return $ randomTweet {tReplyTo = Nothing, tReplies = [], tMentions = []}

-- | Insert Users into given databse
insertUsers :: Config -> [UserName] -> IO ()
insertUsers config users = do
    pool <- runStderrLoggingT $ createSqlitePool (cs $ cfgDevelopmentDBPath config) 5
    runSqlPool (runMigration migrateAll) pool

    forM_ users $ \user ->
        ignoreException $ void $ insertUser pool config user

-- | Exception handling for generator
ignoreException :: IO () -> IO ()
ignoreException = handle handleException
  where
    handleException :: TwitterException -> IO ()
    handleException e = do
        say $ tshow e
        return ()

--- | Insert random data into data
-- If true, it'll insert user data as well.
insertRandomDataIntoEmptyDB :: Config -> Int -> Bool -> IO ()
insertRandomDataIntoEmptyDB cfg numOfTweets shouldInsertUsers = do
    when shouldInsertUsers $ 
       insertUsers cfg testUserList
    tweetRandomly (cfgDevelopmentDBPath cfg) numOfTweets
