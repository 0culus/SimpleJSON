-- from ch05 of Real World Haskell
-- PrettyJSON.hs

module PrettyJSON where

import           PrettyStub

renderJSONValue :: JSONValue -> Doc
renderJSONValue (JSONBool True)     = text "true"
renderJSONValue (JSONBool False)    = text "false"
renderJSONValue JSONNull            = text "null"
renderJSONValue (JSONNumber num)    = double num
renderJSONValue (JSONString str)    = string str