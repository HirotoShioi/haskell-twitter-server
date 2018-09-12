{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( getAllTweets
    , getTweetsByUser
    , getTweetById
    , getUserByName
    , getLatestTweetId
    , insertTweet
    , insertUser
    ) where

import           RIO


import           Control.Monad.Trans.Cont (ContT (..), evalContT)
import           Data.List                (nub)
import           Database.Persist
import           Database.Persist.Sqlite
import qualified RIO.Map                  as M

import           RIO.Time                 (getCurrentTime)

import           Exceptions               (TwitterException (..))
import           Model                    (DBTweet (..), DBTweetId, DBUser (..),
                                           DBUserId, EntityField (..),
                                           Mention (..), Mentions (..),
                                           Reply (..), Tweet (..),
                                           TweetText (..), Unique (..),
                                           User (..), UserName (..),
                                           Validate (..))
import           Util                     (maybeM, whenJust)

import           Configuration            (Config (..))

--------------------------------------------------------------------------------
-- SQL Logic
--------------------------------------------------------------------------------

-- | Default SelectOpt
defaultTweetSelectOpt :: [SelectOpt DBTweet]
defaultTweetSelectOpt = [Desc DBTweetCreatedAt]

filterUnMentionedTweet :: DBUserId -> [Tweet] -> [Tweet]
filterUnMentionedTweet _ [] = []
filterUnMentionedTweet userid (x:xs)
    | fromSqlKey userid `elem` map mId (tMentions x) = 
        x {tReplies = filterUnMentionedTweet userid (tReplies x)} : filterUnMentionedTweet userid xs
    | otherwise = filterUnMentionedTweet userid xs

-- | This is pure, we can test this!
filterTweets :: DBUserId -> Tweet -> Tweet
filterTweets userid tweet = tweet {tReplies = filterUnMentionedTweet userid (tReplies tweet)}

-- | Convert DBTweet record into Tweet type
dbTweetToTweet :: DBUserId -> Entity DBTweet -> SqlPersistM Tweet
dbTweetToTweet userid (Entity tid dbt) = do
    eReplyList <- selectList [ReplyParent ==. tid] [Asc ReplyCreatedAt]
    let replyList = entityVal <$> eReplyList
    replies    <- getTweetsByIdDB userid (map replyChild replyList)
    mentions   <- getMentionList tid
    mUser      <- get $ dBTweetAuthorId dbt
    case mUser of
        Nothing -> throwM $ UserIdNotFound (dBTweetAuthorId dbt)
        Just user -> do
            let tweet = Tweet
                    { tId        = fromSqlKey tid
                    , tText      = TweetText $ dBTweetText dbt
                    , tAuthor    = UserName $ dBUserName user
                    , tCreatedAt = dBTweetCreatedAt dbt
                    , tReplyTo   = fromSqlKey <$> dBTweetReplyTo dbt
                    , tMentions  = mentions
                    , tReplies   = replies
                    }
            return $ filterTweets userid tweet

getMentionList :: DBTweetId -> SqlPersistM [Mention]
getMentionList tid = do
    dbMentionList <- fmap (mentionsUserId . entityVal) <$> selectList [MentionsTweetId ==. tid] []
    mentionedUsers <- getMany dbMentionList
    let mentionList = map
            (\(key, user) ->
                Mention { mName = UserName $ dBUserName user, mId = fromSqlKey key}
            ) (M.toList mentionedUsers)
    return mentionList

-- | Fetch DBTweet with it's id and convert into Tweet type
getTweetByIdDB :: DBUserId -> DBTweetId -> SqlPersistM Tweet
getTweetByIdDB userid tweetNum = do
    mTweet <- getEntity tweetNum
    case mTweet of
        Nothing    -> throwM $ TweetNotFound tweetNum
        Just tweet -> dbTweetToTweet userid tweet

getTweetsByIdDB :: DBUserId -> [DBTweetId] -> SqlPersistM [Tweet]
getTweetsByIdDB userid = mapM (getTweetByIdDB userid)

-- | Query user by it's name
getUserByNameDB :: UserName -> SqlPersistM (Entity DBUser)
getUserByNameDB name = do
    let username = getUserName name
    mUser <- getBy $ UniqueUserName username
    case mUser of
        Nothing    -> throwM $ UserNotFound name
        Just eUser -> return eUser

dbUserToUser :: Entity DBUser -> SqlPersistM User
dbUserToUser (Entity uid dbuser) = do
    userTweets <- selectList [DBTweetAuthorId ==. uid] defaultTweetSelectOpt
    pure User
        { uId             = fromSqlKey uid
        , uName           = UserName (dBUserName dbuser)
        , uNumberOfTweets = length userTweets
        , uFollowers      = 0
        , uFollow         = 0
        , uLikes          = 0
        , uRetweets       = 0
        , uProfile        = "To be implemented"
        }

--------------------------------------------------------------------------------
-- IO Logic
--------------------------------------------------------------------------------

-- | Get all tweets
getAllTweets :: ConnectionPool -> IO [Tweet]
getAllTweets pool =
    flip runSqlPersistMPool pool $ do
        dbtweets <- selectList [DBTweetReplyTo ==. Nothing] defaultTweetSelectOpt
        -- Fix in the future!
        mapM (dbTweetToTweet (toSqlKey 1)) dbtweets

-- | Get tweets with username
getTweetsByUser :: ConnectionPool -> UserName -> IO [Tweet]
getTweetsByUser pool username =
    flip runSqlPersistMPool pool $ do
        eUserId <- try $ entityKey <$> getUserByNameDB username
        case eUserId of
            Left (_ :: TwitterException) -> return []
            Right userId -> do
                dbts <- selectList
                    [ DBTweetAuthorId ==. userId
                    , DBTweetReplyTo  ==. Nothing
                    ]
                    defaultTweetSelectOpt
                mapM (dbTweetToTweet userId) dbts

-- | Get tweet by its Id
getTweetById :: ConnectionPool -> DBTweetId -> IO Tweet
getTweetById pool tweetId =
    flip runSqlPersistMPool pool $ do
        (rootId, rootAuthor) <- findRootId tweetId
        getTweetByIdDB rootAuthor rootId
  where
    findRootId :: DBTweetId -> SqlPersistM (DBTweetId, DBUserId)
    findRootId dbTid = do
        mDBTweet <- get dbTid
        evalContT $ do
            dbTweet  <- mDBTweet !? throwM (TweetNotFound dbTid)
            parentId <- dBTweetReplyTo dbTweet !? return (dbTid, dBTweetAuthorId dbTweet)
            lift $ findRootId parentId
        where
            Nothing !? e = ContT $ const e
            Just a  !? _    = ContT ($ a)

-- | Get user by name
getUserByName :: ConnectionPool -> UserName -> IO User
getUserByName pool userName =
    flip runSqlPersistMPool pool $ do
        eDBUser <- getUserByNameDB userName
        dbUserToUser eDBUser

-- | Insert a tweet
-- if it's an reply tweet, you'll need to provide the parent id with (Maybe Int64)
insertTweet :: ConnectionPool -> UserName -> TweetText -> Maybe Int64 -> [Int64] -> IO Tweet
insertTweet pool postUser content mReplyToInt mentions = do
    let mReplyTo = toSqlKey <$> mReplyToInt
    flip runSqlPersistMPool pool $ do
        currTime  <- getCurrentTime
        ePostUser <- getUserByNameDB postUser
        edbt      <- insertEntity DBTweet
            { dBTweetText      = getTweetText content
            , dBTweetAuthorId  = entityKey ePostUser
            , dBTweetCreatedAt = currTime
            , dBTweetReplyTo   = mReplyTo
            }

        whenJust mReplyTo $ \parentId ->
            maybeM
                (throwM $ ParentTweetNotFound parentId)
                (\tweet -> do
                    insert_ $ Reply parentId (entityKey edbt) currTime
                    void $ insertUnique $ Mentions (entityKey edbt) (dBTweetAuthorId tweet) currTime
                )
                (get parentId)

        -- Update mention table
        let filteredMentions = map toSqlKey $ nub mentions
        mentionedUserIds <- getMany filteredMentions
        let something = fst <$> M.toList mentionedUserIds

        mapM_ (\userId -> void $ insertUnique $ Mentions (entityKey edbt) userId currTime) something

        dbTweetToTweet (entityKey ePostUser) edbt

-- | Insert an user
insertUser :: ConnectionPool -> Config -> UserName -> IO User
insertUser pool config name =
    case validate config name of
        Left e -> throwM e
        Right validName -> do
            let userName = getUserName validName
            flip runSqlPersistMPool pool $ do
                mUser <- getBy $ UniqueUserName userName
                if isJust mUser
                    then throwM $ UserNameAlreadyExists name
                    else do
                        eUser <- insertEntity $ DBUser userName
                        dbUserToUser eUser

-- | Get most recent tweetId
getLatestTweetId :: ConnectionPool -> IO (Maybe Int64)
getLatestTweetId pool =
    flip runSqlPersistMPool pool $ do
        mTweet <- selectFirst [] [Desc DBTweetCreatedAt]
        return $ fromSqlKey . entityKey <$> mTweet

-- | Get random Tweet
