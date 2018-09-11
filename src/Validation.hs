{-# LANGUAGE LambdaCase #-}
module Validation
    ( Validate(..)
    , ValidationException(..)
    ) where

import           RIO

import           Configuration (Config (..))
import           Data.Char     (isAscii)
import           RIO.Text      (unpack)

import           Model         (Content (..), UserName (..))
--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

class Validate a where
    validate :: Config -> a -> Either ValidationException a

instance Validate UserName where
    validate = validateUserName

instance Validate Content where
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
        if length (unpack usrName) >= cfgUserNameMinLength cfg
            then return usrName
            else Left $ UserNameTooShort (cfgUserNameMinLength cfg)
    isValidMaxLength :: Text -> Either ValidationException Text
    isValidMaxLength usrName =
        if length (unpack usrName) <= cfgUserNameMaxLength cfg
            then return usrName
            else Left $ UserNameTooLong (cfgUserNameMaxLength cfg)
    isUsingValidCharacters ::Text -> Either ValidationException Text
    isUsingValidCharacters name =
        if all isAscii $ unpack name
            then return name
            else Left $ InvalidCharacters name

validateContent :: Config -> Content -> Either ValidationException Content
validateContent cfg ccc = do
    let cc = getContent ccc
    Content <$> (isValidContentLength cc >>= isNonEmptyTweet)
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

instance Exception ValidationException

instance Show ValidationException where
    show = \case
        UserNameTooShort num   ->
            "Given user name is too short, must be at least " <> show num <> " characters"
        UserNameTooLong num    ->
            "Given user name is too long, must be shorter than " <> show num <> " characters"
        InvalidCharacters name ->
            "Give user name includes invalid characters: " <> unpack name
        TweetTooLong num       ->
            "Given content is too long, must be shorter than " <> show num <> " characters"
        EmptyTweet             ->
            "Empty content has been given, must include something."
