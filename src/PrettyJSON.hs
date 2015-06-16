-- from ch05 of Real World Haskell
-- PrettyJSON.hs

module PrettyJSON where

import           PrettyStub
import           SimpleJSON

renderJSONValue :: JSONValue -> Doc
renderJSONValue (JSONBool True)     = text "true"
renderJSONValue (JSONBool False)    = text "false"
renderJSONValue JSONNull            = text "null"
renderJSONValue (JSONNumber num)    = double num
renderJSONValue (JSONString str)    = string str

prettyPrintString :: String -> Doc
prettyPrintString = enclose '"' '"' . hcat . map oneChar

-- helpers for pretty printing a string
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right