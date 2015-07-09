-- from ch05 of Real World Haskell
-- Prettify.hs
module Prettify where

import           SimpleJSON ()

-- we store Doc as an algebraic type. Note that this is actually
-- a tree structure
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

text :: String -> Doc
text ""     = Empty
text str    = Text str

double :: Double -> Doc
double num = text (show num)

-- takes a Doc and yields a function of type Doc -> Doc, i.e.
-- takes a Doc and returns a Doc. Analagous to (++), but for
-- concatenating Doc values
(<>) :: Doc -> Doc -> Doc
Empty <> b      = b -- note Empty is the identity under concatenation
a <> Empty      = a
-- this way, we ensure that nothing happens if one or the other
-- arg is empty
a <> b          = a `Concat` b

char :: Char -> Doc
char = Char

-- Takes a list of Doc and returns a Doc
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- Takes any function of type (Doc -> Doc -> Doc) and yields
-- a function that takes a list of Doc and returns a Doc
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- takes a list of Doc and returns a Doc
fsep :: [Doc] -> Doc
fsep = fold (</>)

-- takes a Doc and yields a function of type Doc -> Doc, i.e.
-- takes a Doc and returns a Doc
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> y

softLine :: Doc
softLine = group Line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)      = flatten x `Concat` flatten y
flatten Line                = Char ' '
flatten (x `Union` _)       = flatten x
flatten other               = other

-- takes a Doc and yields a function of type [Doc] -> [Doc],
-- which of course takes a list of Doc and returns a list
-- of Doc
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

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty           -> best col ds
                Char c          -> c : best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line            -> '\n' : best 0 ds
                a `Concat` b    -> best col (a:b:ds)
                a `Union` b     -> nicest col (best col (a:ds))
                                              (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                       | otherwise                  = b
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

-- Exercise 1:
-- It should add spaces to a document until it is the given number of columns wide. If it is
-- already wider than this value, it should add no spaces. [Also refer to the comments on the
-- web version of the book.]
fill :: Int -> Doc -> Doc
fill xs = undefined

-- Exercise 2:
-- Our pretty printer does not take nesting into account. Whenever we open parentheses, braces,
-- or brackets, any lines that follow should be indented so that they are aligned with the
-- opening character until a matching closing character is encountered.
nest :: Int -> Doc -> Doc
nest xs = undefined
