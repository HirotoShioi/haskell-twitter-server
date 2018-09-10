{-# LANGUAGE LambdaCase #-}

module Exceptions
    ( TwitterException (..)
    ) where

import           RIO

import           RIO.Text (unpack)
import           Database.Persist.Sqlite

import           Model (DBTweetId, UserName(..), DBUserId)

data TwitterException =
    TweetNotFound DBTweetId
  | ParentTweetNotFound DBTweetId
  | UserIdNotFound DBUserId
  | UserNotFound UserName
  | UserNameAlreadyExists UserName

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
