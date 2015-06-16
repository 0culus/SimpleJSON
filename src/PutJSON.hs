-- from ch05 of Real World Haskell
-- PutJSON.hs

module PutJSON where

import           Data.List  (intercalate)
import           SimpleJSON

renderJSONValue :: JSONValue -> String
renderJSONValue (JSONString s)      = show s
renderJSONValue (JSONNumber n)      = show n
renderJSONValue (JSONBool True)     = "true"
renderJSONValue (JSONBool False)    = "false"
renderJSONValue JSONNull            = "null"

renderJSONValue (JSONObject o)      = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v)   = show k ++ ": " ++ renderJSONValue v

renderJSONValue (JSONArray a)       = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJSONValue vs)

putJSONValue :: JSONValue -> IO ()
putJSONValue v = putStrLn (renderJSONValue v)
