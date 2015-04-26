-- HW03
module Golf where

import Data.List

-- Exercise 1
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] = [[1]]
-- skips [True,False ] == [[True,False], [False]]
-- skips [] = []
--
-- How it works:
-- data flows right-to-left, ie the argumentw will be applie to zipIndex first
--     unzipIndex <<<< (filterNth n) <<<< zipIndex <<<< INPUT
--
-- Currying the 2-arg function 'filterNth' like this :(filterNth n) 
-- gives us a function with 1-arg that is ready to operatate on [(x,i)]
--
-- unzipIndex then removes the indices from the filtered data
--
skips :: [a] -> [[a]]
skips xs = 
  map
    (\n -> (unzipIndex . (filterNth n) .zipIndex) xs)
    [1..(length xs)]

-- Takes an array and zips it with the its indices (indexed by 1)
-- zipIndex ['a'] == [('a',1)] 
-- zipIndex "a" == [('a',1)] 
-- zipIndex "abc" == [('a',1), ('b',2), ('c',3] 
zipIndex :: [a] ->[(a,Int)]
zipIndex = (\a -> zip a [1..(length a)])

-- Takes 2 argument,
-- arg 1 (n) - for the position to filter
-- arg 2 [(a, i)] - to filter if i is divisible by n 
filterNth :: Integral n => n -> [(c,n)] -> [(c,n)]
filterNth n xs = filter (\(_,i) -> i `mod` n == 0) xs

-- unzipIndex ['a'] == [('a',1)] 
-- unzipIndex "a" == [('a',1)] 
-- unzipIndex "abc" == [('a',1), ('b',2), ('c',3] 
unzipIndex :: [(a,b)] -> [a]
unzipIndex xs = map (\(a,_) -> a) xs

-- Alternate Implementation - Longer without function composition
skips' :: [a] -> [[a]]
skips' a = map (\i -> filterByN i (withIndex a)) [1..(length a)]
  where
    withIndex = (\a -> zip a [1..(length a)])
    mc = map (\(c,i) -> c)
    filterByN  = (\n -> ( mc . (filter (\(c,i) -> i `mod` n == 0))))

-- Example of function composition
zipUnzip :: [a] -> [a]
zipUnzip = unzipIndex . zipIndex

zipFilterUnzip :: Int -> [a] -> [a]
zipFilterUnzip n = (unzipIndex . (filterNth n) . zipIndex)

skips'' :: [a] -> [[a]]
skips'' xs = map (\i -> zipFilterUnzip i xs) [1..(length xs)]

-- test
testSkips :: Bool
testSkips = skips "abcdefghij" == ["abcdefghij","bdfhj","cfi","dh","ej","f","g","h","i","j"]

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Int] -> [Int]
localMaxima xs = ((map snd3) . (filter isLocalMaxima) . zipAdjacents) xs

-- zip the elements on the left and right to create an array of touples 
-- zipAdjacents [a,b,c,d] == [(a,b,c), (b,c,d)]
zipAdjacents :: Integral a =>  [a] -> [(a,a,a)]
zipAdjacents a@(_:xs:xss) = zipWith3 (\a b c -> (a,b,c)) a (xs:xss) xss

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

isLocalMaxima :: (Int,Int,Int) -> Bool
isLocalMaxima (l,x,r) = l < x && x > r

testLocalMaxima :: [Bool]
testLocalMaxima = map 
  (\(input,expected) -> localMaxima input == expected)
  [
    ([2,9,5,6,1], [9,6]),
    ([2,3,4,1,5],[4]),
    ([1,2,3,4,5], [])
  ]

-- Ex 3:
--
histogram :: [Int] -> String
histogram xs = intercalate "\n" (transpose bars)
    where
      bars =  map (\(symbol, counts) -> bar (show symbol) counts) symbolCounts
      symbolCounts = zip [0..9] counts
      maxCount = foldl max 1 counts
      bar = histogramBar maxCount 
      counts = map (\i -> countN i xs) [0..9]

-- calculates the frequency of n in xs
--
countN :: Int -> [Int] -> Int
countN n xs = foldl (\t x -> t + 1) 0 (filter (\i -> i == n) xs)

-- Returns a 'bar' of *'s representing the given symbol and its frequency 
histogramBar :: Int -> String -> Int -> String
histogramBar maxHeight symbol height = reverse (symbol ++ "=" ++ (stars height maxHeight))   

stars :: Int -> Int -> String
stars n maxHeight = pad maxHeight ' ' (take n (repeat '*'))

pad :: Int -> a -> [a] -> [a]
pad n a xs = xs ++ take (n - (length xs)) (repeat a)


-- test
testHistogram :: IO()
testHistogram = putStr (histogram [1,1,2,3,4,5,5])
-- undefined -- [1,1,1,5] ==  undefined


{--
*
* **
 ==========
 0123456789
--}
