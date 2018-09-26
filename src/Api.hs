{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Api
Description : This module contains servant endpoints
Copyright   : (c) Hiroto Shioi, 2018
License     : GPL-3
Maintainer  : shioihigg@email.com
Stability   : experimental
Portability : POSIX

This module describes 'Servant' endpoints needed for the server
-}

module Api
    ( Api
    , api
    ) where

import           RIO

import           Servant

import           Lib     (Sorted)
import           Model   (Tweet, User, UserName)


-- | Twitter endpoint
type Api =
         "api" :> "user"   :> "tweets"  :> Capture "user" UserName :> Get '[JSON] (Sorted [Tweet])
    :<|> "api" :> "user"   :> "profile" :> Capture "user" UserName :> Get '[JSON] User
    :<|> "api" :> "user"   :> Capture "user" UserName :> Post '[JSON] User
    :<|> "api" :> "tweet"  :> Capture "id" Int64 :> Get '[JSON] (Sorted Tweet)

-- | Api
api :: Proxy Api
api = Proxy
