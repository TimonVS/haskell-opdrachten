import Assignment_One

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