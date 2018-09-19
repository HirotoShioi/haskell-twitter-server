module Configuration
    ( Config(..)
    , defaultConfig
    , PortNumber
    ) where

import           RIO

import           Database.Persist.Postgresql

-- | Configuration
data Config = Config {
       cfgPortNumber        :: !PortNumber
     -- ^ Port number used for server
     , cfgConnectionString  :: !ConnectionString
     -- ^ Connection string
     , cfgTweetLength       :: !Int
     -- ^ Length of an tweet
     , cfgUserNameMinLength :: !Int
     -- ^ Minimun length of an username
     , cfgUserNameMaxLength :: !Int
     -- ^ Maximum length for an username
     , cfgServerName        :: !Text
     -- ^ Name of the server
     }

type PortNumber = Int

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config {
      cfgPortNumber        = 3000
    , cfgConnectionString  = connStr
    , cfgTweetLength       = 140
    , cfgUserNameMinLength = 3
    , cfgUserNameMaxLength = 20
    , cfgServerName        = "Haskell Twitter server"
    }

-- | Replace with yaml file!!
connStr :: ConnectionString    
connStr = "host=localhost dbname=perservant user=test password=test port=5432"

-- version: '3.1'

-- services:
--   postgres:
--     image: postgres
--     ports:
--       - '127.0.0.1:5432:5432'
--     environment:
--       - POSTGRES_PASSWORD=test
--       - POSTGRES_USER=test
--       - POSTGRES_DB=perservant
--     volumes:
--       - perservant-db:/var/lib/postgresql/data:rw

-- volumes:
--   perservant-db: