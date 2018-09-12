module Util
    ( safeHead
    , whenJust
    , maybeM
    ) where

import           RIO

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x
