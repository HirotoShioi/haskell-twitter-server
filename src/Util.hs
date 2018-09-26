{-|
Module      : Util
Description : Collection of utility functions
Copyright   : (c) Hiroto Shioi, 2018
License     : GPL-3
Maintainer  : shioihigg@email.com
Stability   : experimental
Portability : POSIX

This module contains utility functions that are useful.
-}

module Util
    ( safeHead
    , whenJust
    , maybeM
    , eitherM
    ) where

import           RIO

-- | Safe head
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Perform some operation on 'Just', given the field inside the 'Just'.
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | Monadic 'maybe'
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic generalisation of 'either'.
eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM l r x = either l r =<< x
