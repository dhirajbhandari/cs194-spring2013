-- Ex:1
-- toDigits 1234 == [1,2,3,4]
-- toDigitsRev 1234 == [4,3,2,1]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 0     = []
  | x == 0    = []
  | otherwise = lst:(toDigitsRev rest)
  where
    lst = x `mod` 10
    rest = x `div` 10

-- Ex:2
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherRev (reverse x))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:(x1:xs)) = x:(x1 * 2 :(doubleEveryOtherRev xs)) 
doubleEveryOtherRev xs = xs

-- Ex:3
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sumIntDigits x
sumDigits (x:xs) = (sumIntDigits x) + (sumDigits xs)

sumIntDigits :: Integer -> Integer
sumIntDigits x
   | x < 10 = x
   | otherwise = sumDigits (toDigits x)

-- Ex:3 Alternate Implementation
sumAllDigits :: [Integer] -> Integer
sumAllDigits [] = 0
sumAllDigits (x:xs)
  | x < 10 =  x + (sumAllDigits xs)
  | otherwise  = (sumAllDigits (toDigits x)) + (sumAllDigits xs)

-- Ex:4
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
