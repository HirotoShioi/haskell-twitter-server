{-# LANGUAGE RecordWildCards #-}

module Configuration
    ( Config(..)
    , Env(..)
    , defaultConfig
    , PortNumber
    , setupConfig
    ) where

import           RIO

import           Data.Aeson                  (FromJSON (..), withObject, (.:))
import           Data.Yaml                   (decodeFileEither)
import           Database.Persist.Postgresql (ConnectionString, ConnectionPool)
import           Say

import           Util                        (eitherM)

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

-- | Make connection string based upon DBConfig
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

data Env = Env {
      envLogFunc :: !LogFunc
    , envConfig  :: !Config
    , envPool    :: !ConnectionPool
    }

instance HasLogFunc Env where
    logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

-- | Setup configuration
setupConfig :: IO Config
setupConfig = do
    let config = defaultConfig
    say "Reading config file"

    dbConfig <- eitherM throwM return (decodeFileEither "database.yaml")

    let connstr = mkConnStr dbConfig
    let configWithConnStr = config {cfgConnectionString = connstr}
    return configWithConnStr

