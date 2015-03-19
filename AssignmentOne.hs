{-|
INFSEN-01-1 - Assignment one
By Erik Schamper (0864951, INF3C),
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
-- | Checks if a 'Char' is a digit.
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

-- | Takes a list and returns a list with no duplicate elements
unique :: (Eq a) => [a] -> [a]
unique xs = unique' xs []
	where
		unique' [] _ = []
		unique' (x:xs) ls
			| x `elem` ls = unique' xs ls
			| otherwise = x : unique' xs (x:ls)

-- | Quicksort algorithm
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []  
quicksort (x:xs) =   
    let smallerList = quicksort [a | a <- xs, a <= x]  
        biggerList = quicksort [a | a <- xs, a > x]  
    in  smallerList ++ [x] ++ biggerList  

-- 1.
-- | Checks if a 'String' contains a digit.
containsDecimal :: String -> Bool
containsDecimal = any isDigit

-- 2.
-- | Returns a list of decimals in a 'String'.
-- Recursively goes through a 'String' and uses 'span' to group digits.
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
-- | Returns the longest decimal in a 'String'
longestDecimal :: String -> Int
longestDecimal = longestDecimal' . getDecimalsAsString
	where
		longestDecimal' [x] = length x
		longestDecimal' (x:xs) = max (length x) (longestDecimal' xs)

-- 5.
-- | Returns 'maximum' decimal in a 'String'
maxDecimal :: String -> Int
maxDecimal = maximum . getDecimals

-- 6.
-- | Takes two lists and returns a list containing only elements that appear in both lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = unique [x | x <- xs, x `elem` ys]

-- 7.
-- | Takes two lists and returns a list containing only elements that appear in one of the lists.
disjunction :: (Eq a) => [a] -> [a] -> [a]
disjunction xs ys = (uniqueElements xs ys) ++ (uniqueElements ys xs)
	where
		uniqueElements as bs = unique [a | a <- as, not (a `elem` bs)]

-- 8.
-- | Takes two 'String's and checks whether or not they are a permutation of eachother. 
-- Does a quicksort on both input lists and compares them
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation as bs
	| length as /= length bs = False
	| otherwise = quicksort as == quicksort bs

-- 9.
-- | Takes two strings and returns a 'String' containing characters that appear an equal amount of times in both strings.
-- First sorts both strings, groups the same characters together and performs an intersection on these groups.
equalCount :: String -> String -> String
equalCount xs ys = map head $ group (quicksort xs) `intersection` group (quicksort ys)
	where
		group [] = []
		group (z:zs) = 
			let (as, bs) = span (== z) zs
			in (z:as) : group bs

-- 10.
-- | Takes a list and returns a list of lists where every element n is a list of every nth element in the input list.
-- Starts with the length of the list as the divisor and recursively decrements this.
-- For every disivor pass, the indices of the list are looped and the element at this index added to the result if the index can be divided by the divisor.
elken :: [a] -> [[a]]
elken xs = reverse $ elken' xs (length xs)
	where
		elken' _ 0 = []
		elken' xs d = [xs !! (i - 1) | i <- [1..length xs], i `mod` d == 0] : elken' xs (d - 1)

-- 11.
-- | Takes a list and returns a list of every local maximum.
-- A local maximum is where an element is greater than both its neighbours.
-- The list is recursively looped and every element compared to its neighbours.
locMax :: (Eq a, Ord a) => [a] -> [a]
locMax [] = []
locMax (x:[]) = []
locMax (x:y:[]) = []
locMax (x:y:z:xs)
	| y > x && y > z = y : locMax (z:xs)
	| otherwise = locMax (y:z:xs)
