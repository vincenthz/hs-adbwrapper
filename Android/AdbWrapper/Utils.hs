module Android.AdbWrapper.Utils where

import Data.Char (isSpace)
import Data.List

skipTill :: (a -> Bool) -> [a] -> [a]
skipTill _ []     = []
skipTill f (x:xs)
    | f x       = skipTill f xs
    | otherwise = x:xs

stripStart :: (Eq a, Show a) => [a] -> [a] -> [a]
stripStart p s
    | sLen < pLen      = error ("expecting prefix " ++ show p ++ " in " ++ show s)
    | p `isPrefixOf` s = drop pLen s
    | otherwise        = error ("expecting prefix " ++ show p ++ " in " ++ show s)
  where pLen = length p
        sLen = length s

stripSpaces :: [Char] -> [Char]
stripSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
