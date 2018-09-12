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
import           Database.Persist
import           Database.Persist.Sqlite
import           RIO.Time                 (getCurrentTime)

import           Exceptions               (TwitterException (..))
import           Model                    (Content (..), DBTweet (..),
                                           DBTweetId, DBUser (..),
                                           EntityField (..), Reply (..),
                                           Tweet (..), Unique (..), User (..),
                                           UserName (..), Validate (..))
import           Util                     (whenJust)

import           Configuration            (Config (..))

--------------------------------------------------------------------------------
-- SQL Logic
--------------------------------------------------------------------------------

-- | Default SelectOpt
defaultTweetSelectOpt :: [SelectOpt DBTweet]
defaultTweetSelectOpt = [Desc DBTweetCreatedAt]

-- | Convert DBTweet record into Tweet type
dbTweetToTweet :: Entity DBTweet -> SqlPersistM Tweet
dbTweetToTweet (Entity tid dbt) = do
    eReplyList <- selectList [ReplyParent ==. tid] [Asc ReplyCreatedAt]
    let replyList = entityVal <$> eReplyList
    replies    <- getTweetsByIdDB (map replyChild replyList)
    mUser      <- get $ dBTweetAuthorId dbt
    case mUser of
        Nothing -> throwM $ UserIdNotFound (dBTweetAuthorId dbt)
        Just user ->
            return Tweet
                { tId        = fromSqlKey tid
                , tContent   = Content $ dBTweetContent dbt
                , tAuthor    = UserName $ dBUserName user
                , tCreatedAt = dBTweetCreatedAt dbt
                , tReplyTo   = fromSqlKey <$> dBTweetReplyTo dbt
                , tReplies   = replies
                }

-- | Fetch DBTweet with it's id and convert into Tweet type
getTweetByIdDB :: DBTweetId -> SqlPersistM Tweet
getTweetByIdDB tweetNum = do
    mTweet <- getEntity tweetNum
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

dbUserToUser :: Entity DBUser -> SqlPersistM User
dbUserToUser (Entity uid dbuser) = do
    userTweets <- selectList [DBTweetAuthorId ==. uid] defaultTweetSelectOpt
    pure User
        { uName           = UserName (dBUserName dbuser)
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
        mapM dbTweetToTweet dbtweets

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
                mapM dbTweetToTweet dbts

-- | Get tweet by its Id
getTweetById :: ConnectionPool -> DBTweetId -> IO Tweet
getTweetById pool tweetId =
    flip runSqlPersistMPool pool $ do
        rootId <- findRootId tweetId
        getTweetByIdDB rootId
  where
    findRootId :: DBTweetId -> SqlPersistM DBTweetId
    findRootId dbTid = do
        mDBTweet <- get dbTid
        evalContT $ do
            dbTweet <- mDBTweet !? throwM (TweetNotFound dbTid)
            parentId <- dBTweetReplyTo dbTweet !? return dbTid
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
insertTweet :: ConnectionPool -> UserName -> Content -> Maybe Int64 -> IO Tweet
insertTweet pool name content mReplyToInt = do
    let mReplyTo = toSqlKey <$> mReplyToInt
    flip runSqlPersistMPool pool $ do
        currTime <- getCurrentTime
        eUser    <- getUserByNameDB name
        edbt     <- insertEntity DBTweet
            { dBTweetContent   = getContent content
            , dBTweetAuthorId  = entityKey eUser
            , dBTweetCreatedAt = currTime
            , dBTweetReplyTo   = mReplyTo
            }

        whenJust mReplyTo $ \parentId -> do
                mTweet <- get parentId
                if isJust mTweet
                    then insert_ $ Reply parentId (entityKey edbt) currTime
                    else throwM $ ParentTweetNotFound parentId
        dbTweetToTweet edbt

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
