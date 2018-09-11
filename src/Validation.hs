module Validation where

import           RIO

import           Data.Char
import           RIO.Text (unpack)

import           Configuration (Config(..))
import           Model (UserName (..), Content (..))

isValidUserName :: Config -> UserName -> Bool
isValidUserName cfg userName =
    let name = getUserName userName
    in isValidLength cfg name 
       && isUsingValidCharacters name
  where
    isValidLength :: Config -> Text -> Bool
    isValidLength config name = length (unpack name) <= cfgUserNameLength config
    isUsingValidCharacters ::Text -> Bool
    isUsingValidCharacters name = all isAscii $ unpack name

isValidTweet :: Config -> Content -> Bool
isValidTweet cfg content =
    let contentText = getContent content
    in length (unpack contentText) <= cfgTweetLength cfg
