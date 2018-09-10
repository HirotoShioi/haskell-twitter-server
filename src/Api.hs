{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Api
    , api
    ) where

import           RIO

import           Servant

import           Model (Tweet, DBUser, UserName)

type Api =
         "api" :> "tweets" :> Get '[JSON] [Tweet]
    :<|> "api" :> "user"   :> Capture "user" UserName :> Get '[JSON] [Tweet]
    :<|> "api" :> "user"   :> Capture "user" UserName :> Post '[JSON] DBUser
    :<|> "api" :> "tweet"  :> Capture "id" Int64 :> Get '[JSON] Tweet

api :: Proxy Api
api = Proxy
