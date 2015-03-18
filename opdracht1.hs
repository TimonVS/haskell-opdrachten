import Data.List
import Data.Char
import Data.Maybe
import Data.Bits
import Data.List.Split

isDigit' :: Char -> Bool
isDigit' x = x `elem` ['0'..'9']

containsDecimal :: String -> Bool
containsDecimal [] = False
containsDecimal (x:xs)
	| isDigit x = True
	| otherwise = containsDecimal xs

getDecimalsAsString :: String -> [String]
getDecimalsAsString xs = wordsBy (\x -> not (isDigit' x) .|. isSeparator x) xs

getDecimals :: String -> [Int]
getDecimals xs = map read $ (getDecimalsAsString xs) :: [Int]

decimalCount :: String -> Int
decimalCount xs = length (getDecimalsAsString xs)

longestDecimal :: String -> Int
longestDecimal xs = read $ (maximum (getDecimalsAsString xs)) :: Int