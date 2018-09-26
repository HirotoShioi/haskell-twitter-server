{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Configuration
Description : All the configuration are defined here
Copyright   : (c) Hiroto Shioi, 2018
License     : GPL-3
Maintainer  : shioihigg@email.com
Stability   : experimental
Portability : POSIX

This module contains all the configuration needed to start an server
-}

module Configuration
    ( Config(..)
    , Env(..)
    , defaultConfig
    , PortNumber
    ) where

import           RIO

import           Data.Aeson                  (FromJSON (..), withObject, (.:))
import           Data.Yaml                   (decodeFileEither)
import           Database.Persist.Postgresql (ConnectionString, ConnectionPool)
import           Say

import           Util                        (eitherM)

-- | Configuration of an application
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

-- | PortNumber used for server
type PortNumber = Int

-- | Default configuration
defaultConfig' :: Config
defaultConfig' = Config {
      cfgPortNumber        = 3000
    , cfgConnectionString  = "This is connection string"
    , cfgTweetLength       = 140
    , cfgUserNameMinLength = 3
    , cfgUserNameMaxLength = 20
    , cfgServerName        = "Haskell Twitter server"
    }

-- | Database configuration
-- Need Test, Prod database..
data DBConfig = DBConfig {
      cfgHost     :: !String
    , cfgDBName   :: !String
    , cfgUser     :: !String
    , cfgPassword :: !String
    , cfgDBPort   :: !PortNumber
    }

-- | Environment for the server
data Env = Env {
      envLogFunc :: !LogFunc
    -- ^ 'LogFunc' used for logging within application
    , envConfig  :: !Config
    -- ^ Basic configuration of an application
    , envPool    :: !ConnectionPool
    -- ^ 'ConnectionPool' used to connect to the database
    }

instance HasLogFunc Env where
    logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
      
-- | Create 'ConnectionString' based upon 'DBConfig'
mkConnStr :: DBConfig -> ConnectionString
mkConnStr DBConfig{..} = fromString $
    concat ["host="
           , cfgHost
           , " dbname="
           , cfgDBName
           , " user="
           , cfgUser
           , " password="
           , cfgPassword
           , " port="
           , show cfgDBPort
           ]

instance FromJSON DBConfig where
     parseJSON = withObject "database configuration" $ \o -> do
         host     <- o .: "host"
         dbname   <- o .: "dbname"
         user     <- o .: "user"
         password <- o .: "password"
         port     <- o .: "port"

         pure $ DBConfig host dbname user password port

-- | Setup default configuration
defaultConfig :: IO Config
defaultConfig = do
    let config = defaultConfig'
    say "Reading config file"

    dbConfig <- eitherM throwM return (decodeFileEither "database.yaml")

    let connstr = mkConnStr dbConfig
    let configWithConnStr = config {cfgConnectionString = connstr}
    return configWithConnStr

