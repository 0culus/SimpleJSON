-- from ch05 of Real World Haskell
-- SimpleJSON.hs
module SimpleJSON
    (
      JSONValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

-- algebraic data type to represent all possible JSON types
data JSONValue =  JSONString String
                | JSONNumber Double
                | JSONBool   Bool
                | JSONNull
                | JSONObject [(String, JSONValue)]
                | JSONArray [JSONValue]
                  deriving (Eq, Ord, Show)

-- now some helpers
getString :: JSONValue -> Maybe String
getString (JSONString s) = Just s
getString _              = Nothing

getInt (JSONNumber n) = Just (truncate n)
getInt _              = Nothing

getDouble (JSONNumber n) = Just n
getDouble _              = Nothing

getBool (JSONBool b) = Just b
getBool _            = Nothing

getObject (JSONObject o) = Just o
getObject _              = Nothing

getArray (JSONArray a) = Just a
getArray _             = Nothing

isNull v               = v == JSONNull
