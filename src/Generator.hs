{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator
    ( tweetRandomly
    , insertUsers
    , insertRandomDataIntoEmptyDB
    ) where

import           RIO

import           Configuration           (Config (..))
import           Control.Lens            ((&), (.~))
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.String.Conversions (cs)
import           Database.Persist.Sqlite
import           Exceptions              (TwitterException (..))
import           Lib                     (getLatestTweetId, getTweetById,
                                          getUserLists, insertTweet, insertUser)
import           Model                   (Tweet (..), TweetText (..),
                                          UserName (..), migrateAll, tAuthor,
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
            randomlyFetchedTweet <- getTweetById pool (toSqlKey randomId)

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
                    , getTweetText (randomReply ^. tText)
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
insertRandomDataIntoEmptyDB :: Config -> Bool -> Int -> IO ()
insertRandomDataIntoEmptyDB cfg shouldInsertUsers numOfTweets = do
    when shouldInsertUsers $
       insertUsers cfg testUserList
    tweetRandomly (cfgDevelopmentDBPath cfg) numOfTweets
