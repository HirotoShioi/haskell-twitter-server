{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Api
    , api
    ) where

import           RIO

import           Servant

import           Model (Tweet, User, UserName)

type Api =
         "api" :> "tweets" :> Get '[JSON] [Tweet]
    :<|> "api" :> "user"   :> "tweets"  :> Capture "user" UserName :> Get '[JSON] [Tweet]
    :<|> "api" :> "user"   :> "profile" :> Capture "user" UserName :> Get '[JSON] User
    -- Replace this API with something better
    :<|> "api" :> "user"   :> Capture "user" UserName :> Post '[JSON] User
    :<|> "api" :> "tweet"  :> Capture "id" Int64 :> Get '[JSON] Tweet

api :: Proxy Api
api = Proxy
