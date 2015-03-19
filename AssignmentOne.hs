{-|
INFSEN-01-1 - Assignment one
By Erik Schamper (, INF3C),
Timon van Spronsen (0866142, INF3C)
|-}

module AssignmentOne
( isDigit  
, containsDecimal
, getDecimalsAsString
, getDecimals
, decimalCount
, longestDecimal
, maxDecimal
, unique
, intersection
, disjunction
, quicksort
, isPermutation
, equalCount
, elken
, locMax
) where 

-- Helper functions
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

-- 1.
-- |Checks if a 'String' contains a digit.
containsDecimal :: String -> Bool
containsDecimal [] = False
containsDecimal (x:xs)
	| isDigit x = True
	| otherwise = containsDecimal xs

-- 2.
-- | 
getDecimalsAsString :: String -> [String]
getDecimalsAsString [] = []
getDecimalsAsString (x:xs)
	| isDigit x = (x:ys) : getDecimalsAsString zs
	| otherwise = getDecimalsAsString xs
	where (ys, zs) = span isDigit xs

-- | Convert 'String' to a list of 'Int'.
getDecimals :: String -> [Int]
getDecimals xs = map read $ (getDecimalsAsString xs) :: [Int]

-- 3.
-- | Returns 'length' of 'getDecimalsAsString'
decimalCount :: String -> Int
decimalCount = length . getDecimalsAsString

-- 4.
-- | 
longestDecimal :: String -> Int
longestDecimal = longestDecimal' . getDecimalsAsString
	where
		longestDecimal' [x] = length x
		longestDecimal' (x:xs) = max (length x) (longestDecimal' xs)

-- 5.
-- | Returns 'maximum' decimal from a 'String'
maxDecimal :: String -> Int
maxDecimal = maximum . getDecimals

-- 6.
-- | 
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
-- |
disjunction :: String -> String -> String
disjunction xs ys = (uniqueChars xs ys) ++ (uniqueChars ys xs)
	where
		uniqueChars as bs = unique [a | a <- as, not (a `elem` bs)]

-- 8.
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerList = quicksort [a | a <- xs, a <= x]  
        biggerList = quicksort [a | a <- xs, a > x]  
    in  smallerList ++ [x] ++ biggerList  

-- | Does a quicksort on both input lists and compares them
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation as bs
	| length as /= length bs = False
	| otherwise = quicksort as == quicksort bs

-- 9.
-- | 
equalCount :: String -> String -> String
equalCount xs ys = map head [x | x <- group (quicksort xs), x `elem` group (quicksort ys)]
	where
		group [] = []
		group (z:zs) = 
			let (as, bs) = span (== z) zs
			in (z:as) : group bs


-- 10.
-- | 
elken :: [a] -> [[a]]
elken xs = reverse $ elken' xs (length xs)
	where
		elken' _ 0 = []
		elken' xs d = [xs !! (i - 1) | i <- [1..length xs], i `mod` d == 0] : elken' xs (d - 1)

-- 11.
-- | 
locMax :: (Eq a, Ord a) => [a] -> [a]
locMax [] = []
locMax (x:[]) = []
locMax (x:y:[]) = []
locMax (x:y:z:xs)
	| y > x && y > z = y : locMax (z:xs)
	| otherwise = locMax (y:z:xs)
