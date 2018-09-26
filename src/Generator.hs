{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Generator
Description : Generator used to generate random users/tweets
Copyright   : (c) Hiroto Shioi, 2018
License     : GPL-3
Maintainer  : shioihigg@email.com
Stability   : experimental
Portability : POSIX

This modules is used to generate random data and insert them into the database.
This is useful to test the endpoints.
-}

module Generator
    ( insertRandomDataIntoEmptyDB
    ) where

import           RIO

import           Control.Lens                ((&), (.~))
import           Control.Monad.Logger        (runNoLoggingT)

import           Database.Persist.Postgresql
import qualified RIO.Text                    as T
import           Say                         (say)
import           Test.QuickCheck             (Gen, arbitrary, choose, elements,
                                              generate, vectorOf)

import           Configuration               (Config (..), Env (..),
                                              defaultConfig)
import           Exceptions                  (TwitterException (..))
import           Lib                         (getLatestTweetId, getSorted,
                                              getTweetById, getUserLists,
                                              insertTweet, insertUser)
import           Model                       (Tweet (..), TweetText (..),
                                              UserName (..),
                                              ValidationException, migrateAll,
                                              tAuthor, tMentions, tReplies,
                                              tReplyTo, tText, testUserList)

--------------------------------------------------------------------------------
-- Random generator to facilitate data insertion
--------------------------------------------------------------------------------

-- | Tweet type
data TweetType =  Normal | Response

-- | Insert given number of random 'Tweet' into the database
tweetRandomly :: Int -> RIO Env ()
tweetRandomly num = do
     randomList <- liftIO $ generate $ vectorOf num (elements [Normal, Response])

     forM_ randomList $ \case
        Normal   -> insertRandomTweet
        Response -> replyRandomTweet

-- | Insert random 'Tweet'
insertRandomTweet :: RIO Env ()
insertRandomTweet = do
    randomTweet <- liftIO $ generate mkRandomTweet
    let userName = randomTweet ^. tAuthor
        content  = randomTweet ^. tText
    ignoreException $ void $ insertTweet userName content Nothing []

-- | Reply to random 'Tweet'
replyRandomTweet :: RIO Env ()
replyRandomTweet = do
    -- Need to fetch random tweet
    latestTweetId <- getLatestTweetId
    case latestTweetId of
        Nothing   -> insertRandomTweet
        (Just num) -> do
            -- Get random tweet
            randomId <- liftIO $ generate $ elements [1 .. (fromSqlKey num)]
            randomlyFetchedTweet <- getSorted <$> getTweetById (toSqlKey randomId)

            -- Generate random reply
            randomReply <- liftIO $ generate mkRandomTweet

            -- Fetch list of users with their ids in tuple (UserName, DBUserId)
            userLists <- getUserLists
            numOfUsers <- liftIO $ generate $ choose (0, 3)
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
                void $ insertTweet 
                       postUser 
                       (TweetText content) 
                       (Just $ toSqlKey randomId)
                       mentionedUserIds

-- | Generate random 'Tweet' with no replies and parentId
mkRandomTweet :: Gen Tweet
mkRandomTweet = do
    randomTweet <- arbitrary :: Gen Tweet
    return $ randomTweet
        & tReplyTo .~ Nothing
        & tReplies .~ []
        & tMentions .~ []

-- | Insert list of 'User' into given database
insertUsers :: [UserName] -> RIO Env ()
insertUsers users = do
    pool <- envPool <$> ask
    liftIO $ runSqlPool (runMigration migrateAll) pool

    forM_ users $ \user ->
        ignoreException $ void $ insertUser user

-- | Exception handling for generator
ignoreException :: RIO Env () -> RIO Env ()
ignoreException action = catches action
    [Handler handleTwitterException, Handler handleValidationException]
  where
    handleTwitterException :: TwitterException -> RIO Env ()
    handleTwitterException = handleException
    handleValidationException :: ValidationException -> RIO Env ()
    handleValidationException = handleException
    handleException :: (Exception e) => e -> RIO Env ()
    handleException e = do
        say $ tshow e
        return ()

-- | Insert given number of random 'Tweet' as well userdata to the database
--
-- If true, it'll insert 'User' data as well.
insertRandomDataIntoEmptyDB :: Bool -> Int -> IO ()
insertRandomDataIntoEmptyDB shouldInsertUsers numOfTweets = do
    config  <- defaultConfig
    pool    <- liftIO $ runNoLoggingT $ createPostgresqlPool (cfgConnectionString config) 5
    logOpts <- logOptionsHandle stdout False

    liftIO $ withLogFunc logOpts $ \logfunc -> do
        let env = Env logfunc config pool

        when shouldInsertUsers $ runRIO env (insertUsers testUserList)

        runRIO env (tweetRandomly numOfTweets)
