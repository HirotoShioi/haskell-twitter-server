{-# LANGUAGE LambdaCase #-}

module Exceptions
    ( TwitterException (..)
    ) where

import           RIO

import           RIO.Text (unpack)
import           Database.Persist.Sqlite

import           Model (DBTweetId, UserName(..), DBUserId)

-- | Exceptions for twitter server
data TwitterException =
    TweetNotFound DBTweetId
  -- ^ Tweet with given Id was not found
  | ParentTweetNotFound DBTweetId
  -- ^ Looked up parent tweet but it was not found
  | UserIdNotFound DBUserId
  --- ^ Looked up user by its Id but it was not found
  | UserNotFound UserName
  -- ^ User with given name was not found
  | UserNameAlreadyExists UserName
  -- ^ Try to register user but the username is already used

instance Exception TwitterException

instance Show TwitterException where
    show = \case
        TweetNotFound dtid -> 
            "Tweet with given id was not found: " <> show (fromSqlKey dtid)
        ParentTweetNotFound dtid -> 
            "Parent tweet was not found: " <> show (fromSqlKey dtid)
        UserIdNotFound uid ->
            "User with given id was not found: " <> show (fromSqlKey uid)
        UserNotFound name ->
            "User with given name was not found: " <> unpack (getUserName name)
        UserNameAlreadyExists name ->
            "User with given name already exists: " <> unpack (getUserName name)
