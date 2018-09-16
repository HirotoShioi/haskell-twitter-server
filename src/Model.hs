{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Warning regarding Orphan instance is too annoying
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           RIO                     hiding ((^.))

import           Control.Lens            (makeLenses, (^.))
import           Data.Aeson              (ToJSON (..), object, (.=))
import           Data.Char               (isAscii)

import           Database.Persist.Sqlite (Key, fromSqlKey, toSqlKey)
import           Database.Persist.TH

import qualified RIO.Text                as T
import           RIO.Time                (UTCTime (..), fromGregorian)

import           Servant                 (FromHttpApiData (..))
import           Test.QuickCheck         (Arbitrary (..), Gen, choose, elements,
                                          vectorOf)

import           Configuration           (Config (..))

--------------------------------------------------------------------------------
-- Database Schema
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- | Database representation of an user
DBUser
    name Text
    UniqueUserName name
    deriving Show
-- | Database representation of an tweet
DBTweet
    text Text
    authorId DBUserId
    createdAt UTCTime default=CURRENT_TIME
    replyTo DBTweetId Maybe
    deriving Show
-- | Relational table for replies
Reply
    parent DBTweetId
    child  DBTweetId
    createdAt UTCTime default=CURRENT_TIME
-- | Mention table
Mentions
    tweetId DBTweetId
    userId  DBUserId
    UniqueMention tweetId userId
    mentionedAt UTCTime default=CURRENT_TIME
|]

--------------------------------------------------------------------------------
-- DataTypes
--------------------------------------------------------------------------------

-- | User name
newtype UserName = UserName
    { getUserName :: Text
    } deriving (Show, Eq)

-- | Tweet text
newtype TweetText = TweetText
    { getTweetText :: Text
    } deriving (Show, Eq)

-- | Mentions
data Mention = Mention {
      _mName :: !UserName
    -- ^ Name of the user mentioned
    , _mId   :: !DBUserId
    -- ^ Id of an user mentioned
    } deriving Show

makeLenses ''Mention

-- | Endpoint representaiton of DBTweet data
-- Perhaps user lens just to practice them
data Tweet = Tweet
    { _tId        :: !DBTweetId
    -- ^ Int64 representation of tweet Id
    , _tText      :: !TweetText
    -- ^ Content aka tweet itself
    , _tAuthor    :: !UserName
    -- ^ Author of the tweet
    , _tCreatedAt :: !UTCTime
    -- ^ Date in which the tweet was created at
    , _tReplyTo   :: !(Maybe DBTweetId)
    -- ^ Mentions
    , _tMentions  :: ![Mention]
    -- ^ Id of parent tweet
    , _tReplies   :: ![Tweet]
    -- ^ List of replies
    } deriving (Show)

makeLenses ''Tweet

-- | User datatype
data User = User
    { _uId             :: !DBUserId
    -- ^ UserId
    , _uName           :: !UserName
    -- ^ Name of the user
    , _uNumberOfTweets :: !Int
    -- ^ Number of tweets
    , _uFollowers      :: !Int
    -- ^ Number of followers
    , _uFollow         :: !Int
    -- ^ Number of follows
    , _uLikes          :: !Int
    -- ^ Number of tweets user liked
    , _uRetweets       :: !Int
    -- ^ Number of retweets
    , _uProfile        :: !Text
    -- ^ Short text describing the user
    } deriving Show

makeLenses ''User

--------------------------------------------------------------------------------
-- TypeClasses
--------------------------------------------------------------------------------

instance ToJSON Tweet where
    toJSON t =
        let tweetObj = object
                [ "tweet_id"   .= fromSqlKey (t ^. tId)
                , "text"       .= getTweetText (t ^. tText)
                , "author"     .= getUserName (t ^. tAuthor)
                , "created_at" .= (t ^. tCreatedAt)
                , "reply_to"   .= (fromSqlKey <$> t ^. tReplyTo)
                , "replies"    .= (t ^. tReplies)
                , "mentions"   .= (t ^. tMentions)
                ]
        in object ["tweet" .= tweetObj]

instance ToJSON User where
    toJSON u =
        let userObj = object
                [ "user_id"             .= fromSqlKey (u ^. uId)
                , "username"            .= getUserName (u ^. uName)
                , "number_of_tweets"    .= (u ^. uNumberOfTweets)
                , "number_of_followers" .= (u ^. uFollowers)
                , "number_of_follows"   .= (u ^. uFollow)
                , "number_of_likes"     .= (u ^. uLikes)
                , "number_of_retweets"  .= (u ^. uRetweets)
                , "profile"             .= (u ^. uProfile)
                ]
        in object ["user" .= userObj]

instance ToJSON Mention where
    toJSON m =
        object [ "user_name" .= getUserName (m ^. mName)
               , "user_id"   .= fromSqlKey (m ^. mId)
               ]

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary TweetText where
    arbitrary = TweetText <$> arbitrary

instance Arbitrary UserName where
    arbitrary = UserName <$> arbitrary

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

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

instance Arbitrary DBUserId where
    arbitrary = toSqlKey <$> arbitrary

instance Arbitrary Mention where
    arbitrary = do
        _mName <- elements testUserList
        _mId   <- arbitrary

        pure Mention{..}

instance Arbitrary Tweet where
    arbitrary = do
        _tId        <- arbitrary
        _tText      <- TweetText <$> elements
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
        _tAuthor    <- elements testUserList
        _tCreatedAt <- arbitrary
        _tReplyTo   <- arbitrary
        _tMentions  <- arbitrary
        listLen     <- choose (0,2)
        _tReplies   <- vectorOf listLen arbitrary

        pure Tweet{..}

instance Arbitrary User where
    arbitrary = do
        _uId             <- arbitrary
        _uName           <- elements testUserList
        _uNumberOfTweets <- arbitrary
        _uFollowers      <- choose (1, 1000)
        _uFollow         <- choose (1, 1000)
        _uLikes          <- choose (1, 100)
        _uRetweets       <- choose (1, 1000)
        _uProfile        <- elements
           [ "I'm mathmatician"
           , "I'm from Kyoto"
           , "I drunk too many yesterday"
           , "Geocachin' everyday"
           , "京都のHaskeller"
           , "Hello from Barbados"
           ]
        pure User{..}

-- | List of users used for testing
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

-- | Typeclass for validation
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
    -- ^ Exception for username being too short
    | UserNameTooLong Int
    -- ^ Exception for username being too long
    | InvalidCharacters Text
    -- ^ Invalid character was user for an username
    | TweetTooLong Int
    -- ^ Tweet text was too long
    | EmptyTweet
    -- ^ Empty tweet was given

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
