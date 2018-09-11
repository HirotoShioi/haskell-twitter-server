module Validation where

import           RIO

import           RIO.Text                (unpack)
import           Data.Char               (isAscii)
import           Configuration (Config (..))
import           Model  (Content (..), UserName (..))
--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- Maybe access database?
-- Nahhh valid username and name already existing on db is different problem
validateUserName :: Config -> Text -> Either ValidationException UserName
validateUserName cfg userName =
    UserName <$> (isValidMinLength userName >>= isValidMaxLength >>= isUsingValidCharacters)
  where
    isValidMinLength :: Text -> Either ValidationException Text
    isValidMinLength name =
        if length (unpack name) >= cfgUserNameMinLength cfg
            then return name
            else Left $ UserNameTooShort (cfgUserNameMinLength cfg)
    isValidMaxLength :: Text -> Either ValidationException Text
    isValidMaxLength name =
        if length (unpack name) <= cfgUserNameMaxLength cfg
            then return name
            else Left $ UserNameTooLong (cfgUserNameMaxLength cfg)
    isUsingValidCharacters ::Text -> Either ValidationException Text
    isUsingValidCharacters name =
        if all isAscii $ unpack name
            then return name
            else Left $ InvalidCharacters name

validateContent :: Config -> Text -> Either ValidationException Content
validateContent cfg content = Content <$> (isValidContentLength content >>= isNonEmptyTweet)
  where
    isValidContentLength :: Text -> Either ValidationException Text
    isValidContentLength c =
        if length (unpack c) <= cfgTweetLength cfg
            then return c
            else Left $ TweetTooLong (cfgTweetLength cfg)
    isNonEmptyTweet :: Text -> Either ValidationException Text
    isNonEmptyTweet c =
        if not $ null (unpack c)
            then return c
            else Left EmptyTweet

data ValidationException =
      UserNameTooShort Int
    | UserNameTooLong Int
    | InvalidCharacters Text
    | TweetTooLong Int
    | EmptyTweet
    deriving Show

instance Exception ValidationException
