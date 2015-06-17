-- from ch05 of Real World Haskell
-- Prettify.hs
module Prettify where

import SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line

string :: String -> Doc
string str = undefined

text :: String -> Doc
text ""     = Empty
text str    = Text str

double :: Double -> Doc
double num = text (show num)

(<>) :: Doc -> Doc -> Doc
Empty <> b      = b
a <> Empty      = a
a <> b          = a `Concat` b

char :: Char -> Doc
char = Char

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep xs = undefined

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> any

softLine :: Doc
softLine = group Line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)      = flatten x `Concat` flatten any
flatten Line                = Char ' '
flatten (x `Union` _)       = flatten x
flatten other               = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

-- this could be made more idiomatic, as with a lot more :P
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
                case d of
                    Empty        -> transform ds
                    Char c       -> c : transform ds
                    Text s       -> s ++ transform ds
                    Line         -> '\n' : transform ds
                    a `Concat` b -> transform (a:b:ds)
                    _ `Union` b  -> transform (b:ds)
