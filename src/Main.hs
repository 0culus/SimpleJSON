-- from ch05 of Real World Haskell
-- Main.hs

module Main where

import           PutJSON
import           SimpleJSON

main :: IO ()
main = putJSONValue (JSONObject [("foo", JSONNumber 1), ("bar", JSONBool False),
    ("test", JSONArray [JSONNumber 12, JSONNumber 24, JSONNumber 36]),
    ("chars", JSONArray [JSONString "this", JSONString "is", JSONString "an", JSONString "array"])])
