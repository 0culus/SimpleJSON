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

-- lookup seems to be O(n)...gotta be a better way...maybe later?
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    -- is this really good Haskell? Exposing the underlying
    -- representation of char...
    -- also, `c == '\x7f'`: Error:(31, 43) <qcon> or <qvar> expected, got '\'?
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

-- helpers for proper escaping
-- an association list /'alist'/
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
            <> text (replicate (4 - length h) '0')
            <> text h
    where h = showHex x ""

-- wrinkle: the above only works for unicode up to 0xffff
-- time for some bit manipulation
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
