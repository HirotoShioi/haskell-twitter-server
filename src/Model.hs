{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Warning regarding Orphan instance is too annoying
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           RIO

import           Data.Aeson              (ToJSON (..), object, (.=))
import           Data.Char               (isAscii)

import           Database.Persist.Sqlite (Key, toSqlKey)
import           Database.Persist.TH

import qualified RIO.Text as T
import           RIO.Time                (UTCTime (..), fromGregorian)

import           Servant                 (FromHttpApiData (..))
import           Test.QuickCheck         (Arbitrary (..), Gen, choose, elements,
                                          vectorOf)

import           Configuration           (Config (..))

-- | Database Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBUser
    name Text
    UniqueUserName name
    deriving Show
DBTweet
    text Text
    authorId DBUserId
    createdAt UTCTime default=CURRENT_TIME
    replyTo DBTweetId Maybe
    deriving Show
Reply
    parent DBTweetId
    child  DBTweetId
    createdAt UTCTime default=CURRENT_TIME
Mentions
    tweetId DBTweetId
    userId  DBUserId
    UniqueMention tweetId userId
    mentionedAt UTCTime default=CURRENT_TIME
|]

newtype UserName = UserName
    { getUserName :: Text
    } deriving (Show, Eq)

newtype TweetText = TweetText
    { getTweetText :: Text
    } deriving (Show, Eq)

newtype TweetId = TweetId
    { getTweetId :: Int64
    } deriving (Show, Eq)

newtype UserId = UserId
    { getUserId :: Int64
    } deriving (Show, Eq)

-- | Endpoint representaiton of DBTweet data
-- Perhaps user lens just to practice them
data Tweet = Tweet
    { tId        :: !TweetId
    -- ^ Int64 representation of tweet Id
    , tText      :: !TweetText
    -- ^ Content aka tweet itself
    , tAuthor    :: !UserName
    -- ^ Author of the tweet
    , tCreatedAt :: !UTCTime
    -- ^ Date in which the tweet was created at
    , tReplyTo   :: !(Maybe TweetId)
    -- ^ Mentions
    , tMentions  :: ![Mention]
    -- ^ Id of parent tweet
    , tReplies   :: ![Tweet]
    -- ^ List of replies
    } deriving (Show)

data Mention = Mention {
      mName :: !UserName
    , mId   :: !UserId
    } deriving Show

-- (TODO) Create User type
data User = User
    { uId             :: !UserId
    -- ^ UserId
    , uName           :: !UserName
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
    -- ^ Short text describing the user
    } deriving Show
--------------------------------------------------------------------------------
-- TypeClasses
--------------------------------------------------------------------------------

instance ToJSON Tweet where
    toJSON Tweet{..} =
        let tweetObj = object
                [ "tweet_id"  .= getTweetId tId
                , "text"      .= getTweetText tText
                , "author"    .= getUserName tAuthor
                , "createdAt" .= tCreatedAt
                , "replyTo"   .= (getTweetId <$> tReplyTo)
                , "replies"   .= tReplies
                , "mentions"  .= tMentions
                ]
        in object ["tweet" .= tweetObj]

instance ToJSON User where
    toJSON User{..} =
        let userObj = object
                [ "username"       .= getUserName uName
                , "numberOfTweets" .= uNumberOfTweets
                ]
        in object ["user" .= userObj]

instance ToJSON Mention where
    toJSON Mention{..} =
        object [ "user_name" .= getUserName mName
               , "user_id"   .= getUserId mId
               ]

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary TweetText where
    arbitrary = TweetText <$> arbitrary

instance Arbitrary UserName where
    arbitrary = UserName <$> arbitrary

instance Arbitrary TweetId where
    arbitrary = TweetId <$> arbitrary

instance Arbitrary UserId where
    arbitrary = UserId <$> arbitrary

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

instance Arbitrary Mention where
    arbitrary = do
        mName <- elements testUserList
        mId   <- arbitrary

        pure Mention{..}

instance Arbitrary Tweet where
    arbitrary = do
        tId        <- arbitrary
        tText      <- TweetText <$> elements
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
        tMentions  <- arbitrary
        listLen    <- choose (0,2)
        tReplies   <- vectorOf listLen arbitrary

        pure Tweet{..}

instance Arbitrary User where
    arbitrary = do
        uId   <- arbitrary
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
    ["Hiroto", "HumbleMumble", "Ana"
    , "Dudo", "Charles", "Alan", "McSherry"]

--------------------------------------------------------------------------------
-- FromHttpApiData
--------------------------------------------------------------------------------

instance FromHttpApiData UserName where
    parseUrlPiece userName = UserName <$> parseUrlPiece userName

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

class Validate a where
    validate :: Config -> a -> Either ValidationException a

instance Validate UserName where
    validate = validateUserName

instance Validate TweetText where
    validate = validateContent

-- Maybe access database?
-- Nahhh valid username and name already existing on db is different problem
validateUserName :: Config -> UserName -> Either ValidationException UserName
validateUserName cfg userName = do
    let name = getUserName userName
    UserName <$> (isValidMinLength name >>= isValidMaxLength >>= isUsingValidCharacters)
  where
    isValidMinLength :: Text -> Either ValidationException Text
    isValidMinLength usrName =
        if T.length usrName >= cfgUserNameMinLength cfg
            then return usrName
            else Left $ UserNameTooShort (cfgUserNameMinLength cfg)
    isValidMaxLength :: Text -> Either ValidationException Text
    isValidMaxLength usrName =
        if T.length usrName <= cfgUserNameMaxLength cfg
            then return usrName
            else Left $ UserNameTooLong (cfgUserNameMaxLength cfg)
    isUsingValidCharacters ::Text -> Either ValidationException Text
    isUsingValidCharacters name =
        if all isAscii $ T.unpack name
            then return name
            else Left $ InvalidCharacters name

validateContent :: Config -> TweetText -> Either ValidationException TweetText
validateContent cfg ccc = do
    let cc = getTweetText ccc
    TweetText <$> (isValidContentLength cc >>= isNonEmptyTweet)
  where
    isValidContentLength :: Text -> Either ValidationException Text
    isValidContentLength c =
        if T.length c <= cfgTweetLength cfg
            then return c
            else Left $ TweetTooLong (cfgTweetLength cfg)
    isNonEmptyTweet :: Text -> Either ValidationException Text
    isNonEmptyTweet c =
        if not $ T.null c
            then return c
            else Left EmptyTweet

data ValidationException =
      UserNameTooShort Int
    | UserNameTooLong Int
    | InvalidCharacters Text
    | TweetTooLong Int
    | EmptyTweet

instance Exception ValidationException

instance Show ValidationException where
    show = \case
        UserNameTooShort num   ->
            "Given user name is too short, must be at least " <> show num <> " characters"
        UserNameTooLong num    ->
            "Given user name is too long, must be shorter than " <> show num <> " characters"
        -- Buggy,, how do I fix it?
        InvalidCharacters name ->
            "Give user name includes invalid characters: " <> show name
        TweetTooLong num       ->
            "Given content is too long, must be shorter than " <> show num <> " characters"
        EmptyTweet             ->
            "Empty content has been given, must include something."
