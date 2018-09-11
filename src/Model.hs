{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Warning regarding Orphan instance is too annoying
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           RIO

import           Data.Aeson              (ToJSON (..), object, (.=))
import           Database.Persist.Sqlite (Key, toSqlKey)
import           Database.Persist.TH

import           RIO.Time                (UTCTime (..), fromGregorian)
import           Servant                 (FromHttpApiData (..))
import           Test.QuickCheck         (Arbitrary (..), Gen, choose, elements,
                                          vectorOf)

-- | Database Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBUser
    name Text
    UniqueUserName name
    deriving Show
DBTweet
    content Text
    authorId DBUserId
    createdAt UTCTime default=CURRENT_TIME
    replyTo DBTweetId Maybe
    deriving Show
Reply
    parent DBTweetId
    child  DBTweetId
    createdAt UTCTime default=CURRENT_TIME
|]

newtype UserName = UserName
    { getUserName :: Text
    } deriving (Show, Eq)

newtype Content = Content
    { getContent :: Text
    } deriving (Show, Eq)

-- | Endpoint representaiton of DBTweet data
-- Perhaps user lens just to practice them
data Tweet = Tweet
    { tId        :: !Int64
    -- ^ Int64 representation of tweet Id
    , tContent   :: !Content
    -- ^ Content aka tweet itself
    , tAuthor    :: !UserName
    -- ^ Author of the tweet
    , tCreatedAt :: !UTCTime
    -- ^ Date in which the tweet was created at
    , tReplyTo   :: !(Maybe Int64)
    -- ^ Id of parent tweet
    , tReplies   :: ![Tweet]
    -- ^ List of replies
    } deriving (Show)

-- (TODO) Create User type
data User = User
    { uName           :: !UserName
    -- ^ Name of the user
    , uNumberOfTweets :: !Int
    -- ^ Number of tweets
    , uFollowers      :: !Int
    -- ^ Number of followers
    , uFollow         :: !Int
    -- ^ Number of follows
    , uLikes          :: !Int
    -- ^ Number of tweets user liked
    , uRetweets       :: !Int
    -- ^ Number of retweets
    , uProfile        :: !Text
    } deriving Show

--------------------------------------------------------------------------------
-- TypeClasses
--------------------------------------------------------------------------------

instance ToJSON Tweet where
    toJSON Tweet{..} =
        let tweetObj = object
                [ "id"        .= tId
                , "content"   .= getContent tContent
                , "author"    .= getUserName tAuthor
                , "createdAt" .= tCreatedAt
                , "replyTo"   .= tReplyTo
                , "replies"   .= tReplies
                ]
        in object ["tweet" .= tweetObj]

instance ToJSON User where
    toJSON User{..} =
        let userObj = object
                [ "username"       .= getUserName uName
                , "numberOfTweets" .= uNumberOfTweets
                ]
        in object ["user" .= userObj]

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary Content where
    arbitrary = Content <$> arbitrary

instance Arbitrary UserName where
    arbitrary = UserName <$> arbitrary

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

-- https://gist.github.com/agrafix/2b48ec069693e3ab851e
instance Arbitrary UTCTime where
    arbitrary =
        do randomDay   <- choose (1, 29) :: Gen Int
           randomMonth <- choose (1, 12) :: Gen Int
           randomYear  <- choose (2001, 2018) :: Gen Integer
           randomTime  <- choose (0, 86401) :: Gen Int
           return $ UTCTime
               (fromGregorian randomYear randomMonth randomDay)
               (fromIntegral randomTime)

instance Arbitrary (Key DBTweet) where
    arbitrary = toSqlKey <$> arbitrary

instance Arbitrary Tweet where
    arbitrary = do
        tId        <- arbitrary
        tContent   <- Content <$> elements
            [ "My first tweet"
            , "Today is rainy day"
            , "My shoulder hurts"
            , "Our football team won the game!"
            , "What was the score?"
            , "Going cyclying to Nara"
            , "Kyoto is very beautiful place"
            , "Haskell in Barbados!"
            , "Alan is drunk!"
            ]
        tAuthor    <- elements testUserList
        tCreatedAt <- arbitrary
        tReplyTo   <- arbitrary
        listLen    <- choose (0,2)
        tReplies   <- vectorOf listLen arbitrary

        pure Tweet{..}

instance Arbitrary User where
    arbitrary = do
        uName <- elements testUserList
        uNumberOfTweets <- arbitrary
        uFollowers <- choose (1, 1000)
        uFollow    <- choose (1, 1000)
        uLikes     <- choose (1, 100)
        uRetweets  <- choose (1, 1000)
        uProfile   <- elements [ "I'm mathmatician"
                               , "I'm from Kyoto"
                               , "I drunk too many yesterday"
                               , "Geocachin' everyday"
                               , "京都のHaskeller"
                               , "Hello from Barbados"
                               ]
        pure User{..}

testUserList :: [UserName]
testUserList = map UserName
    ["Hiroto", "Hiroto.hs", "Ana"
    , "Dudo", "Charles", "Alan", "McSherry"]

--------------------------------------------------------------------------------
-- FromHttpApiData
--------------------------------------------------------------------------------

instance FromHttpApiData UserName where
    parseUrlPiece userName = UserName <$> parseUrlPiece userName