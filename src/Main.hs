-- from ch05 of Real World Haskell
-- Main.hs

module Main where

import SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
