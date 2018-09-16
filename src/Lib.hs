{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( getTweetsByUser
    , getTweetById
    , getUserByName
    , getLatestTweetId
    , getUserLists
    , insertTweet
    , insertUser
    , isTweetSorted
    ) where

import           RIO

import           Control.Lens             ((%~))
import           Control.Monad.Trans.Cont (ContT (..), evalContT)

import           Data.List                (maximum, nub, sortBy)
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
                                           Validate (..), mId, tMentions,
                                           tReplies, tCreatedAt)
import           Util                     (maybeM, whenJust)

import           Configuration            (Config (..))

--------------------------------------------------------------------------------
-- Polishing logics (sort, filter)
--------------------------------------------------------------------------------

-- | Default SelectOpt
defaultTweetSelectOpt :: [SelectOpt DBTweet]
defaultTweetSelectOpt = [Desc DBTweetCreatedAt]

filterUnMentionedTweet :: DBUserId -> [Tweet] -> [Tweet]
filterUnMentionedTweet _ [] = []
filterUnMentionedTweet userid (x:xs)
    | userid `elem` map (^. mId) (x ^. tMentions) =
        [x & tReplies %~ filterUnMentionedTweet userid]
        <> filterUnMentionedTweet userid xs
    | otherwise = filterUnMentionedTweet userid xs

-- | This is pure, we can test this!
filterTweets :: DBUserId -> Tweet -> Tweet
filterTweets userid tweet = tweet & tReplies %~ filterUnMentionedTweet userid

-- | Get max number of an given Tweet
getMostRecentBy :: (Ord a) => (Tweet -> a) -> Tweet -> (a, Tweet)
getMostRecentBy getter tweet =
    let currentSomething = getter tweet
        tweetIds         = map (fst . getMostRecentBy getter) (tweet ^. tReplies)
        -- We definatly have currentSomething so this should never fail
        maxSomething     = maximum (currentSomething:tweetIds)

    in (maxSomething, tweet)

-- | Sort list of tweets with given getter a
-- This is polymorphic meaning we can sort the list with any given getter as long as it has
-- Ord instance
-- So many maps being used so I've assume efficiency is not great
-- Since we're reversing the list, we might need diffList?
sortTweetsBy :: (Ord a) => (Tweet -> a) -> [Tweet] -> [Tweet]
sortTweetsBy _ []      = []
sortTweetsBy getter ts =
    let tweetWithSortedChild = map (\t -> t & tReplies %~ sortTweetsBy getter) ts
        tweetsWithIds        = map (getMostRecentBy getter) tweetWithSortedChild
        sortedTweets         = sortBy (flip (\(aId, _) (bId, _) -> aId `compare` bId)) tweetsWithIds
    in map snd sortedTweets

-- | Sort list of tweets with field "tCreatedAt"
sortTweetsByCreatedAt :: [Tweet] -> [Tweet]
sortTweetsByCreatedAt = sortTweetsBy (^. tCreatedAt)

-- | Sort an given tweet
sortTweetByCreatedAt :: Tweet -> Tweet
sortTweetByCreatedAt t = t & tReplies %~ sortTweetsByCreatedAt 

-- | Check if the tweet is sorted with given getter
isTweetSorted :: (Ord a) => (Tweet -> a) -> [Tweet] -> Bool
isTweetSorted _       []      = True
isTweetSorted getter [x]      = isTweetSorted getter (x ^. tReplies)
isTweetSorted getter (a:b:ts) =
       getter a >= getter b
    && isTweetSorted getter (concatMap (^. tReplies) [a,b])
    && isTweetSorted getter ts

--------------------------------------------------------------------------------
-- SQL Logic
--------------------------------------------------------------------------------

-- | Convert DBTweet record into Tweet type
dbTweetToTweet :: Bool -> DBUserId -> Entity DBTweet -> SqlPersistM Tweet
dbTweetToTweet shouldGetReplies userid (Entity tid dbt) = do
    eReplyList <- selectList [ReplyParent ==. tid] [Asc ReplyCreatedAt]
    let replyList = entityVal <$> eReplyList
    replies    <- if shouldGetReplies
                  then getTweetsByIdDB shouldGetReplies userid (map replyChild replyList)
                  else return []
    mentions   <- getMentionList tid
    mUser      <- get $ dBTweetAuthorId dbt
    case mUser of
        Nothing -> throwM $ UserIdNotFound (dBTweetAuthorId dbt)
        Just user -> do
            let tweet = Tweet
                    { _tId        = tid
                    , _tText      = TweetText $ dBTweetText dbt
                    , _tAuthor    = UserName $ dBUserName user
                    , _tCreatedAt = dBTweetCreatedAt dbt
                    , _tReplyTo   = dBTweetReplyTo dbt
                    , _tMentions  = mentions
                    , _tReplies   = replies
                    }
            return $ sortTweetByCreatedAt $ filterTweets userid tweet

dbTweetsToTweets :: Bool -> DBUserId -> [Entity DBTweet] -> SqlPersistM [Tweet]
dbTweetsToTweets shouldGetReplies userid edbts = do
    unsortedReplies <- mapM (dbTweetToTweet shouldGetReplies userid) edbts
    return $ sortTweetsByCreatedAt unsortedReplies

getMentionList :: DBTweetId -> SqlPersistM [Mention]
getMentionList tid = do
    -- Get list of mentioned user's keys
    dbMentionList <- map (mentionsUserId . entityVal) <$> selectList [MentionsTweetId ==. tid] []
    mentionedUsers <- getMany dbMentionList
    let mentionList = map
            (\(key, user) ->
                Mention { _mName = UserName $ dBUserName user, _mId = key}
            ) (M.toList mentionedUsers)
    return mentionList

-- | Fetch DBTweet with it's id and convert into Tweet type
getTweetByIdDB :: Bool -> DBUserId -> DBTweetId -> SqlPersistM Tweet
getTweetByIdDB shouldGetReplies userid tweetNum = do
    mTweet <- getEntity tweetNum
    case mTweet of
        Nothing    -> throwM $ TweetNotFound tweetNum
        Just tweet -> dbTweetToTweet shouldGetReplies userid tweet

-- | Fetch tweet with given Id
getTweetsByIdDB :: Bool -> DBUserId -> [DBTweetId] -> SqlPersistM [Tweet]
getTweetsByIdDB shouldGetReplies userid = mapM (getTweetByIdDB shouldGetReplies userid)

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
        { _uId             = uid
        , _uName           = UserName (dBUserName dbuser)
        , _uNumberOfTweets = length userTweets
        , _uFollowers      = 0
        , _uFollow         = 0
        , _uLikes          = 0
        , _uRetweets       = 0
        , _uProfile        = "To be implemented"
        }

--------------------------------------------------------------------------------
-- IO Logic
--------------------------------------------------------------------------------

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
                dbTweetsToTweets True userId dbts

-- | Get tweet by its Id
getTweetById :: ConnectionPool -> DBTweetId -> IO Tweet
getTweetById pool tweetId =
    flip runSqlPersistMPool pool $ do
        (rootId, rootAuthor) <- findRootId tweetId
        getTweetByIdDB True rootAuthor rootId
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
-- Perhaps replace userName with userId?
-- Needs validation on TweetText
insertTweet :: ConnectionPool
            -> UserName
            -> TweetText
            -> Maybe DBTweetId
            -> [DBUserId]
            -> IO Tweet
insertTweet pool postUser content mReplyTo mentions =
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
                    void $ insertUnique 
                         $ Mentions (entityKey edbt) (dBTweetAuthorId tweet) currTime
                )
                (get parentId)

        -- Update mention table
        let filteredMentions = map toSqlKey $ nub (map fromSqlKey mentions)
        mentionedUserIds <- getMany filteredMentions
        let userKeys = fst <$> M.toList mentionedUserIds

        forM_ userKeys $ \userId -> 
            void $ insertUnique $ Mentions (entityKey edbt) userId currTime

        dbTweetToTweet True (entityKey ePostUser) edbt

-- | Insert an user with given name
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

--------------------------------------------------------------------------------
-- Generator related
--------------------------------------------------------------------------------

-- | Get most recent tweetId
getLatestTweetId :: ConnectionPool -> IO (Maybe DBTweetId)
getLatestTweetId pool =
    flip runSqlPersistMPool pool $ do
        mTweet <- selectFirst [] [Desc DBTweetCreatedAt]
        return $ entityKey <$> mTweet

-- | Get user ids
getUserLists :: ConnectionPool -> IO [(DBUserId, UserName)]
getUserLists pool =
    flip runSqlPersistMPool pool $ do
        userLists <- selectList [] [Asc DBUserName]
        let userIdNames = map
                (\(Entity k u) -> (k, UserName $ dBUserName u))
                userLists
        return userIdNames
