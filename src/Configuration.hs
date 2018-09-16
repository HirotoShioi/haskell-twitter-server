module Configuration
    ( Config(..)
    , defaultConfig
    , PortNumber
    ) where

import           RIO

-- | Configuration
data Config = Config {
       cfgProductionDBPath  :: !FilePath
     -- ^ Filepath to the production database
     , cfgDevelopmentDBPath :: !FilePath
     -- ^ Filepath to the development databse
     , cfgPortNumber        :: !PortNumber
     -- ^ Port number used for server
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
      cfgProductionDBPath  = "sqlite.db"
    , cfgDevelopmentDBPath = "sqlite.db"
    , cfgPortNumber        = 3000
    , cfgTweetLength       = 140
    , cfgUserNameMinLength = 3
    , cfgUserNameMaxLength = 20
    , cfgServerName        = "Haskell Twitter server"
    }
