import AssignmentOne

{-|
INFSEN-01-1 - Assignment one tests
By Erik Schamper (0864951, INF3C),
Timon van Spronsen (0866142, INF3C)
|-}

-- Test 1.
containsDecimalTest = containsDecimal "hallo123 456 test" == True

-- Test 2.
getDecimalsTest = getDecimals "Hallo123 456 test78" == [123,456,78]

-- Test 3.
decimalCountTest = decimalCount "Hallo123 456 test78" == 3

-- Test 4.
longestDecimalTest = longestDecimal "Hallo123 456 test78 9101112" == 7

-- Test 5.
maxDecimalTest = maxDecimal "Hallo123 456 test78 9101112" == 9101112

-- Test 6.
intersectionTest = intersection "programming" "software" == "roa"

-- Test 7.
disjunctionTest = disjunction "programming" "software" == "pgminsftwe"

-- Test 8.
isPermutationTest = isPermutation [1,2,4,5] [2,4,5,1] == True

-- Test 9.
equalCountTest = equalCount "programming" "software" == "ao"

-- Test 10.
elkenTest = elken "ABCD" == ["ABCD", "BD", "C", "D"]

-- Test 11.
locMaxTest = locMax [2,9,5,6,1] == [9,6]