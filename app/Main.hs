module Main where

import RIO

import Server (run)

main :: IO ()
main = run "sqlite.db"