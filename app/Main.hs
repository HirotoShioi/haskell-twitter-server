module Main where

import           RIO

import           Say
import           Server (run)

main :: IO ()
main = do
    let portNumber = 3000
    say $ "Running server on port: " <> tshow portNumber
    run "sqlite.db" portNumber
