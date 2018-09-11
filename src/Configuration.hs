module Configuration
    ( Config(..)
    , defaultConfig
    ) where

import RIO


data Config = Config {
       cfgDBPath         :: !FilePath
     , cfgPortNumber     :: !PortNumber
     , cfgTweetLength    :: !Int
     , cfgUserNameLength :: !Int
     , cfgServerName     :: !Text
     }

type PortNumber = Int

defaultConfig :: Config
defaultConfig = Config { 
      cfgDBPath         = "sqlite.db"
    , cfgPortNumber     = 3000
    , cfgTweetLength    = 140
    , cfgUserNameLength = 20
    , cfgServerName     = "Haskell Twitter server"
    }