{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( getAllTweets
    , getTweetsByUser
    , getTweetById
    , getLatestTweetId
    , insertTweet
    , insertUser
    ) where

import           RIO

import           Database.Persist
import           Database.Persist.Sqlite

import           RIO.Time

import           Exceptions              (TwitterException (..))
import           Model                   (Content (..), DBTweet (..), DBTweetId,
                                          DBUser (..), EntityField (..),
                                          Reply (..), Tweet (..), Unique (..),
                                          UserName (..))
import           Util                    (whenJust)

---------------------------------------------------------------------
-- SQL Logic
---------------------------------------------------------------------

-- | Default SelectOpt
defaultTweetSelectOpt :: [SelectOpt DBTweet]
defaultTweetSelectOpt = [Desc DBTweetCreatedAt]

-- | Convert DBTweet record into Tweet type
dbTweetToTweet :: Entity DBTweet -> SqlPersistM Tweet
dbTweetToTweet (Entity tid dbt) = do
    eReplyList <- selectList [ReplyParent ==. tid] [Asc ReplyCreatedAt]
    let replyList = entityVal <$> eReplyList
    replies <- getTweetsByIdDB (map replyChild replyList)
    mUser   <- selectFirst [DBUserId ==. dBTweetAuthorId dbt] []
    case entityVal <$> mUser of
        Nothing -> throwM $ UserIdNotFound (dBTweetAuthorId dbt)
        Just user ->
            return Tweet
                { tId        = fromSqlKey tid
                , tContent   = Content $ dBTweetContent dbt
                , tAuthor      = UserName $ dBUserName user
                , tCreatedAt = dBTweetCreatedAt dbt
                , tReplyTo   = fromSqlKey <$> dBTweetReplyTo dbt
                , tReplies   = replies
                }

-- | Fetch DBTweet with it's id and convert into Tweet type
getTweetByIdDB :: DBTweetId -> SqlPersistM Tweet
getTweetByIdDB tweetNum = do
    mTweet <- selectFirst [DBTweetId ==. tweetNum] defaultTweetSelectOpt
    case mTweet of
        Nothing    -> throwM $ TweetNotFound tweetNum
        Just tweet -> dbTweetToTweet tweet

getTweetsByIdDB :: [DBTweetId] -> SqlPersistM [Tweet]
getTweetsByIdDB = mapM getTweetByIdDB

-- | Query user by it's name
getUserByNameDB :: UserName -> SqlPersistM (Entity DBUser)
getUserByNameDB name = do
    let username = getUserName name
    mUser <- getBy $ UniqueUserName username
    case mUser of
        Nothing    -> throwM $ UserNotFound name
        Just eUser -> return eUser

--------------------------------------------------------------------------------
-- IO Logic
--------------------------------------------------------------------------------

getAllTweets :: ConnectionPool -> IO [Tweet]
getAllTweets pool =
    flip runSqlPersistMPool pool $ do
        dbtweets <- selectList [DBTweetReplyTo ==. Nothing] defaultTweetSelectOpt
        mapM dbTweetToTweet dbtweets

getTweetsByUser :: ConnectionPool -> UserName -> IO [Tweet]
getTweetsByUser pool username =
    flip runSqlPersistMPool pool $ do
        eUserId <- try $ entityKey <$> getUserByNameDB username
        case eUserId of
            Left (_ :: TwitterException) -> return []
            Right userId -> do
                dbts <- selectList [DBTweetAuthorId ==. userId] defaultTweetSelectOpt
                mapM dbTweetToTweet dbts

getTweetById :: ConnectionPool -> DBTweetId -> IO Tweet
getTweetById pool tweetId =
    flip runSqlPersistMPool pool $ getTweetByIdDB tweetId

insertTweet :: ConnectionPool -> UserName -> Content -> Maybe Int64 -> IO Tweet
insertTweet pool name content mReplyToInt = do
    let mReplyTo = toSqlKey <$> mReplyToInt
    flip runSqlPersistMPool pool $ do
        time  <- getCurrentTime
        eUser <- getUserByNameDB name
        edbt  <- insertEntity DBTweet
            { dBTweetContent   = getContent content
            , dBTweetAuthorId  = entityKey eUser
            , dBTweetCreatedAt = time
            , dBTweetReplyTo   = mReplyTo
            }

        whenJust mReplyTo $ \parentId -> do
                mTweet <- selectFirst [DBTweetId ==. parentId] []
                if isJust mTweet
                    then insert_ $ Reply parentId (entityKey edbt) time
                    else throwM $ ParentTweetNotFound parentId
        dbTweetToTweet edbt

insertUser :: ConnectionPool -> UserName -> IO DBUser
insertUser pool name = do
    let userName = getUserName name
    flip runSqlPersistMPool pool $ do
        mUser <- selectFirst [DBUserName ==. userName] []
        if isJust mUser
            then throwM $ UserNameAlreadyExists name
            else do
                eUser <- insertEntity $ DBUser userName
                return $ entityVal eUser

getLatestTweetId :: ConnectionPool -> IO (Maybe Int64)
getLatestTweetId pool =
    flip runSqlPersistMPool pool $ do
        mTweet <- selectFirst [] [Desc DBTweetCreatedAt]
        return $ fromSqlKey . entityKey <$> mTweet
