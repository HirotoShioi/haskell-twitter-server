{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator
    ( insertRandomDataIntoEmptyDB
    ) where

import           RIO

import           Configuration           (Config (..), setupConfig)
import           Control.Lens            ((&), (.~))
import           Control.Monad.Logger    (runStderrLoggingT)

import           Database.Persist.Postgresql

import           Exceptions              (TwitterException (..))
import           Lib                     (getLatestTweetId, getSorted,
                                          getTweetById, getUserLists,
                                          insertTweet, insertUser)
import           Model                   (Tweet (..), TweetText (..),
                                          UserName (..), ValidationException,
                                          migrateAll, tAuthor, tMentions,
                                          tReplies, tReplyTo, tText,
                                          testUserList)
import qualified RIO.Text                as T
import           Say                     (say)
import           Test.QuickCheck         (Gen, arbitrary, choose, elements,
                                          generate, vectorOf)

--------------------------------------------------------------------------------
-- Random generator to facilitate data insertion
--------------------------------------------------------------------------------
-- FIX GENERATOR

-- | Tweet type
data TweetType =  Normal | Response

-- | Insert random tweets into the database
tweetRandomly :: ConnectionString -> Int -> IO ()
tweetRandomly conn num = do
     randomList <- generate $ vectorOf num (elements [Normal, Response])
     pool <- runStderrLoggingT $ createPostgresqlPool conn 5

     forM_ randomList $ \case
        Normal   -> insertRandomTweet pool
        Response -> replyRandomTweet pool

-- | Insert random tweet
insertRandomTweet :: ConnectionPool -> IO ()
insertRandomTweet pool = do
    randomTweet <- generate mkRandomTweet
    let userName = randomTweet ^. tAuthor
        content  = randomTweet ^. tText
    ignoreException $ void $ insertTweet pool userName content Nothing []

-- | Reply to random tweet
-- Write better reply (add @Mention)
replyRandomTweet :: ConnectionPool -> IO ()
replyRandomTweet pool = do
    -- Need to fetch random tweet
    latestTweetId <- getLatestTweetId pool
    case latestTweetId of
        Nothing   -> insertRandomTweet pool
        (Just num) -> do
            -- Get random tweet
            randomId <- generate $ elements [1 .. (fromSqlKey num)]
            randomlyFetchedTweet <- getSorted <$> getTweetById pool (toSqlKey randomId)

            -- Generate random reply
            randomReply <- generate mkRandomTweet

            -- Fetch list of users with their ids in tuple (UserName, DBUserId)
            userLists <- getUserLists pool
            numOfUsers <- generate $ choose (0, 3)
            let mentionedUsers = take numOfUsers userLists

            -- Modify content
            let parentAuthor = getUserName $ randomlyFetchedTweet ^. tAuthor
            let mentionedUserNames = map snd mentionedUsers
            let mentionText = foldr (\name acc -> "@" <> getUserName name <> " " <> acc)
                              mempty
                              mentionedUserNames
            let content = T.concat
                    [ "@"
                    , parentAuthor
                    , " "
                    , mentionText
                    , getTweetText $ randomReply ^. tText
                    ]

            -- Insert into db
            let postUser = randomReply ^. tAuthor
            let mentionedUserIds = map fst mentionedUsers
            ignoreException $
                void $ insertTweet pool postUser (TweetText content) (Just $ toSqlKey randomId) mentionedUserIds

-- | Generate random tweet with no replies and parentId
mkRandomTweet :: Gen Tweet
mkRandomTweet = do
    randomTweet <- arbitrary :: Gen Tweet
    return $ randomTweet
        & tReplyTo .~ Nothing
        & tReplies .~ []
        & tMentions .~ []

-- | Insert Users into given databse
insertUsers :: Config -> [UserName] -> IO ()
insertUsers config users = do

    pool <- runStderrLoggingT $ createPostgresqlPool (cfgConnectionString config) 5
    runSqlPool (runMigration migrateAll) pool

    forM_ users $ \user ->
        ignoreException $ void $ insertUser pool config user

-- | Exception handling for generator
ignoreException :: IO () -> IO ()
ignoreException action = catches action
    [Handler handleTwitterException, Handler handleValidationException]
  where
    handleTwitterException :: TwitterException -> IO ()
    handleTwitterException = handleException
    handleValidationException :: ValidationException -> IO ()
    handleValidationException = handleException
    handleException :: (Exception e) => e -> IO ()
    handleException e = do
        say $ tshow e
        return ()

-- | Insert given number of random tweets as well userdata to the database
--
-- If true, it'll insert user data as well.
insertRandomDataIntoEmptyDB :: Bool -> Int -> IO ()
insertRandomDataIntoEmptyDB shouldInsertUsers numOfTweets = do
    config <- setupConfig
    when shouldInsertUsers $
       insertUsers config testUserList
    tweetRandomly (cfgConnectionString config) numOfTweets
