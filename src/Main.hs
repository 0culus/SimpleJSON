-- from ch05 of Real World Haskell
-- Main.hs

module Main where

import           PutJSON
import           SimpleJSON

main :: IO ()
main = print (JSONObject [("foo", JSONNumber 1), ("bar", JSONBool False)])
