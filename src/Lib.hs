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
    , Sorted
    , getSorted
    ) where

import           RIO

import           Control.Lens                ((%~))
import           Control.Monad.Trans.Cont    (ContT (..), evalContT)

import           Data.Aeson
import           Data.List                   (maximum, nub, sortBy)
import           Database.Persist
import           Database.Persist.Postgresql

import qualified RIO.Map                     as M

import           RIO.Time                    (getCurrentTime)

import           Exceptions                  (TwitterException (..))
import           Model                       (DBTweet (..), DBTweetId,
                                              DBUser (..), DBUserId,
                                              EntityField (..), Mention (..),
                                              Mentions (..), Reply (..),
                                              Tweet (..), TweetText (..),
                                              Unique (..), User (..),
                                              UserName (..), Validate (..), mId,
                                              tCreatedAt, tMentions, tReplies)
import           Util                        (maybeM, whenJust)

import           Configuration               (Env(..))

--------------------------------------------------------------------------------
-- Polishing logics (sort, filter)
--------------------------------------------------------------------------------

newtype Sorted a = Sorted {getSorted :: a}

instance (ToJSON a) => ToJSON (Sorted a) where
    toJSON (Sorted a) = toJSON a

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
sortTweetsByCreatedAt :: [Tweet] -> Sorted [Tweet]
sortTweetsByCreatedAt = Sorted . sortTweetsBy (^. tCreatedAt)

-- | Sort an given tweet
sortTweetByCreatedAt :: Tweet -> Sorted Tweet
sortTweetByCreatedAt t = Sorted (t & tReplies %~ sortTweetsBy (^. tCreatedAt))

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
            return $ filterTweets userid tweet

dbTweetsToTweets :: Bool -> DBUserId -> [Entity DBTweet] -> SqlPersistM [Tweet]
dbTweetsToTweets shouldGetReplies userid = mapM (dbTweetToTweet shouldGetReplies userid)

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
getTweetsByUser :: UserName -> RIO Env (Sorted [Tweet])
getTweetsByUser username = do
    logInfo $ "Fetching tweets of an username " <> displayShow (getUserName username)
    tweets <- runWithPool $ do
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
    return $ sortTweetsByCreatedAt tweets

-- | Get tweet by its Id
getTweetById :: DBTweetId -> RIO Env (Sorted Tweet)
getTweetById tweetId = do
    logInfo $ "Fetching tweet with an id of " <> displayShow (fromSqlKey tweetId)
    runWithPool $ do
        (rootId, rootAuthor) <- findRootId tweetId
        sortTweetByCreatedAt <$> getTweetByIdDB True rootAuthor rootId
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
getUserByName :: UserName -> RIO Env User
getUserByName userName = do
    logInfo $ "Fetching user profile: " <> displayShow (getUserName userName)
    runWithPool $ do
        eDBUser <- getUserByNameDB userName
        dbUserToUser eDBUser

-- | Insert a tweet
-- Perhaps replace userName with userId?
-- Needs validation on TweetText
insertTweet :: UserName
            -> TweetText
            -> Maybe DBTweetId
            -> [DBUserId]
            -> RIO Env (Sorted Tweet)
insertTweet postUser content mReplyTo mentions = do
    logInfo $ "Inserting tweet by " <> displayShow (getUserName postUser)
    runWithPool $ do
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

        sortTweetByCreatedAt <$> dbTweetToTweet True (entityKey ePostUser) edbt

-- | Insert an user with given name
insertUser :: UserName -> RIO Env User
insertUser name = do
    logInfo $ "Inserting user " <> displayShow (getUserName name)
    config <- envConfig <$> ask
    case validate config name of
        Left e -> throwM e
        Right validName -> do
            let userName = getUserName validName
            runWithPool $ do
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
getLatestTweetId :: RIO Env (Maybe DBTweetId)
getLatestTweetId = do
    logInfo "Fetching latest tweetId"
    runWithPool $ do
        mTweet <- selectFirst [] [Desc DBTweetCreatedAt]
        return $ entityKey <$> mTweet

-- | Get user ids
getUserLists :: RIO Env [(DBUserId, UserName)]
getUserLists = do
    logInfo "Fetching list of users"
    runWithPool $ do
        userLists <- selectList [] [Asc DBUserName]
        let userIdWithNames = map
                (\(Entity k u) -> (k, UserName $ dBUserName u))
                userLists
        return userIdWithNames

runWithPool :: SqlPersistM a -> RIO Env a
runWithPool action = do
    pool <- envPool <$> ask
    liftIO $ runSqlPersistMPool action pool