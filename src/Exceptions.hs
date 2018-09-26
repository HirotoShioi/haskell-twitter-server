{-# LANGUAGE LambdaCase #-}

{-|
Module      : Exceptions
Description : Defines exception that can occur on a server
Copyright   : (c) Hiroto Shioi, 2018
License     : GPL-3
Maintainer  : shioihigg@email.com
Stability   : experimental
Portability : POSIX

This module defines exceptions that can occur upon running server
-}

module Exceptions
    ( TwitterException (..)
    ) where

import           RIO

import           Database.Persist.Postgresql (fromSqlKey)
import           RIO.Text                    (unpack)

import           Model                       (DBTweetId, DBUserId,
                                              UserName (..))

-- | Exceptions for twitter server
data TwitterException =
    TweetNotFound DBTweetId
  -- ^ Tweet with given Id was not found
  | ParentTweetNotFound DBTweetId
  -- ^ Looked up parent tweet but it was not found
  | UserIdNotFound DBUserId
  -- ^ Looked up user by its Id but it was not found
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
