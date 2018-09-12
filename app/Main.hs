module Main where

import           RIO

import           Server (runTwitterServer)

main :: IO ()
main = runTwitterServer
