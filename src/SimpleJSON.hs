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

getDouble :: JSONValue -> Maybe Double
getDouble (JSONNumber n) = Just n
getDouble _              = Nothing

getBool :: JSONValue -> Maybe Bool
getBool (JSONBool b) = Just b
getBool _            = Nothing

getObject :: JSONValue -> Maybe [(String, JSONValue)]
getObject (JSONObject o) = Just o
getObject _              = Nothing

getArray :: JSONValue -> Maybe [JSONValue]
getArray (JSONArray a) = Just a
getArray _             = Nothing

isNull :: JSONValue -> Bool
isNull v               = v == JSONNull
