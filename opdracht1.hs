import Data.List
import Data.Char
import Data.Maybe
import Data.Bits
import Data.List.Split
import Text.ParserCombinators.ReadP
import Data.Word
import System.Environment

containsDecimal :: String -> Bool
containsDecimal [] = False
containsDecimal (x:xs)
	| isDigit x = True
	| otherwise = containsDecimal xs

getDecimals :: String -> [Int]
getDecimals xs = map read $ (wordsBy (\x -> isLetter x .|. isSeparator x) xs) :: [Int]

decimalCount :: String -> Int
decimalCount xs = length (getDecimals xs)

-- intLength :: Int -> Int
-- intLength i =

-- longestDecimal :: String -> Int
-- longestDecimal (x:xs)
-- 	| 















evenCount :: String -> [Bool]
evenCount xs = [even filterInt | x <- xs, isDigit x, let filterInt = digitToInt x]

convert x = (map (map read . words) . lines) x

convert' x = (map read . words) x

filterTest xs = map (filter (not . isLetter)) (words xs)