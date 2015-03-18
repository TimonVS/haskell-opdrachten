import Data.List
import Data.Char
import Data.Maybe
import Data.Bits
import Data.List.Split

isDigit' :: Char -> Bool
isDigit' x = x `elem` ['0'..'9']

-- 1.
containsDecimal :: String -> Bool
containsDecimal [] = False
containsDecimal (x:xs)
	| isDigit x = True
	| otherwise = containsDecimal xs

-- 2.
getDecimalsAsString :: String -> [String]
getDecimalsAsString xs = wordsBy (\x -> not (isDigit' x) .|. isSeparator x) xs

getDecimals :: String -> [Int]
getDecimals xs = map read $ (getDecimalsAsString xs) :: [Int]

-- 3.
decimalCount :: String -> Int
decimalCount xs = length (getDecimalsAsString xs)

-- 4.
longestDecimal :: String -> Int
longestDecimal xs = read $ (maximum (getDecimalsAsString xs)) :: Int

-- 5.
maxDecimal :: String -> Int
maxDecimal xs = maximum (getDecimals xs)

-- 6.
unique :: (Eq a) => [a] -> [a]
unique xs = unique' xs []
	where
		unique' [] _ = []
		unique' (x:xs) ls
			| x `elem` ls = unique' xs ls
			| otherwise = x : unique' xs (x:ls)

intersection :: String -> String -> String
intersection xs ys = unique [x | x <- xs, x `elem` ys]

-- 7.
disjunction :: String -> String -> String
disjunction xs ys = (uniqueChars xs ys) ++ (uniqueChars ys xs)
	where
		uniqueChars as bs = unique [a | a <- as, not (a `elem` bs)]

-- 8.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation as bs
	| length as /= length bs = False
	| otherwise = length [x | x <- as, x `elem` bs] == length bs
