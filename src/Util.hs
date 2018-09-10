module Util 
    ( safeHead
    , whenJust 
    ) where

import           RIO

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg
