{-|
INFSEN-01-1 - Assignment one
By Erik Schamper (, INF3C),
Timon van Spronsen (0866142, INF3C)
|-}

-- Helper functions
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

-- 1.
--| Checks if a 'String' contains a digit.
containsDecimal :: String -> Bool
containsDecimal [] = False
containsDecimal (x:xs)
	| isDigit x = True
	| otherwise = containsDecimal xs

-- 2.
--| 
getDecimalsAsString :: String -> [String]
getDecimalsAsString [] = []
getDecimalsAsString (x:xs)
	| isDigit x = (x : chain) : getDecimalsAsString (drop (length chain) xs)
	| otherwise = getDecimalsAsString xs
	where chain = takeWhile isDigit xs

--| Convert 'String' to a list of 'Int'.
getDecimals :: String -> [Int]
getDecimals xs = map read $ (getDecimalsAsString xs) :: [Int]

-- 3.
--| Returns 'length' of 'getDecimalsAsString'
decimalCount :: String -> Int
decimalCount = length . getDecimalsAsString

-- 4.
--| 
longestDecimal :: String -> Int
longestDecimal = longestDecimal' . getDecimalsAsString
	where
		longestDecimal' [x] = length x
		longestDecimal' (x:xs) = max (length x) (longestDecimal' xs)

-- 5.
--| Returns 'maximum' decimal from a 'String'
maxDecimal :: String -> Int
maxDecimal = maximum . getDecimals

-- 6.
--| 
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
--|
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

--| Does a quicksort on both input lists and compares them
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation as bs
	| length as /= length bs = False
	| otherwise = quicksort as == quicksort bs

-- 9.
--| 
equalCount :: String -> String -> String
equalCount xs ys = foldr (\i acc -> if letterCount commonLetters xs !! i == letterCount commonLetters ys !! i then (commonLetters !! i) : acc else acc) [] [0..length commonLetters - 1]
	where 
		commonLetters = intersection xs ys
		letterCount ls as = map (\l -> foldr (\x acc -> if x == l then 1 + acc else acc) 0 as) ls

-- 10.
--| 
elken :: [a] -> [[a]]
elken xs = reverse $ elken' xs (length xs)
	where
		elken' _ 0 = []
		elken' xs d = [xs !! (i - 1) | i <- [1..length xs], i `mod` d == 0] : elken' xs (d - 1)

-- 11.
--| 
locMax :: (Eq a, Ord a) => [a] -> [a]
locMax [] = []
locMax (x:[]) = []
locMax (x:y:[]) = []
locMax (x:y:z:xs)
	| y > x && y > z = y : locMax (z:xs)
	| otherwise = locMax (y:z:xs)
