-- Ex:5
-- Tower of Hanoi
-- move n discs from first Peg A to second Peg B via third Peg C ? -> return required moves
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- How to do it?
-- 123A B C
-- 23A 1B C
-- 3A 1B 2C
-- 3A B 12C
-- A 3B 12C
-- 1A 3B 2C
-- 1A 23B C
-- A 123B C
--
-- move n disks given: 123...(n-1),nA B C
-- If n = 1
--   move disk from A->B
-- else # n > 1
--   Given:  
-- 123...(n-1),nA B C
-- 
-- move n-1 disks from A->C
-- nA B 123...(n-1)C
--
-- move nth disk from A->B
-- A nB 123...(n-1)C
--
-- move n-1 disks from C->B
-- A 123...(n-1),nB C

-- type Peg = String
data Peg = A | B | C deriving (Show, Eq)
type Move = (Peg, Peg)

-- Tower of Hanoi world
data PegWorld = PegWorld 
  {
    pwStackA  :: PegStack
  , pwStackB  :: PegStack
  , pwStackC  :: PegStack
  , pwMoves   :: [Move]
  } deriving (Show, Eq)

type PegStack = [Int]

-- move N disks from Peg a to b in the given Pegworld, return resulting PegWorld
move :: Move -> PegWorld -> PegWorld 
move (a,b) w = moveN (a,b,n) w
  where n = length (stackByName w a)

moves :: PegWorld -> [Move]
moves w = reverse (pwMoves w)

------------------------------------
-- internals
------------------------------------
moveN :: (Peg,Peg,Int) -> PegWorld -> PegWorld
moveN (p1,p2,n) w
  | n == 1 = applyMove (p1, p2) w
  | otherwise = afterCB -- moveAB moveN (p3, p2, (n - 1)) moveAB 
  where
    afterAC   = moveN (p1, p3, m) w
    afterAB   = applyMove (p1, p2) afterAC
    afterCB   = moveN (p3, p2, m) afterAB
    p3        = otherPeg p1 p2 w
    m         = n - 1


applyMove :: Move -> PegWorld -> PegWorld
applyMove (x,y) w@(PegWorld s1 s2 s3 moves) = w3
  where
    (n:xs)  = stackByName w x 
    ys      = n:(stackByName w y) 
    w3      = modifyWorld w (x, xs) (y, ys)

modifyWorld :: PegWorld -> (Peg, PegStack) -> (Peg, PegStack) -> PegWorld
modifyWorld w (x, xs) (y,ys) = PegWorld as bs cs ((x,y):(pwMoves w))
 where 
  as = if x == A then xs else if y == A then ys else pwStackA w
  bs = if x == B then xs else if y == B then ys else pwStackB w
  cs = if x == C then xs else if y == C then ys else pwStackC w

otherPeg :: Peg -> Peg -> PegWorld -> Peg
otherPeg x y _ = head ([a|a<- [A,B,C], not (a `elem` [x,y])])

stackByName :: PegWorld -> Peg -> [Int]
stackByName w A = pwStackA w
stackByName w B = pwStackB w
stackByName w C = pwStackC w


---------------------------------------------------------------------
-- Verification & Testing
---------------------------------------------------------------------

-- convinience Methods
startWorld :: PegWorld 
startWorld = createWorld 2

createWorld :: Int -> PegWorld 
createWorld n = PegWorld [1..n] [] [] []


validWorld :: PegWorld -> PegWorld
validWorld w = PegWorld (validStack (pwStackA w)) (validStack (pwStackB w)) (validStack (pwStackC w)) (pwMoves w)

validStack :: PegStack -> PegStack
validStack [] = []
validStack (x:[]) = (x:[])
validStack (x:xs:xss)
  | x < xs = (x:validStack (xs:xss))
  | otherwise = error("Stack " ++ show (x:xs:xss) ++ " is Invalid. " ++ show x ++ " is NOT less than " ++ show xs)



testAll :: (Bool, String)
testAll = foldl joinResults (True, "") allTests
  where 
    test1 = testCase 1 [(A,B)]
    test2 = testCase 2 [(A,C), (A,B), (C,B)]
    test3 = testCase 3 [(A,B),(A,C),(B,C),(A,B),(C,A),(C,B),(A,B)]
    allTests = [test1, test2, test3]
    joinResults (x,y) (a,b) = (x && a, y ++ "\n" ++ b)  


testCase :: Int -> [Move] -> (Bool, String)
testCase inputSize expectedMoves
 | passed    = (True, "PASSED " ++ message)
 | otherwise = (False, "FAIELD " ++ message)
 where
  actualMoves = moves finalWorld 
  finalWorld  = testRun inputSize
  passed      = actualMoves == expectedMoves && validateMoves startWorld actualMoves
  message     = "Expected: " ++ show expectedMoves ++  " Actual: " ++ show actualMoves
  startWorld  = createWorld inputSize 


validateMoves :: PegWorld -> [Move] -> Bool
validateMoves startWorld [] = True
validateMoves startWorld (x:xs) =
  let 
     validateWorld (PegWorld a b c _) = (validateStack a) && (validateStack b) && (validateStack c)
     w1 = applyMove x startWorld
  in (validateWorld w1) && (validateMoves w1 xs)


validateStack :: PegStack -> Bool
validateStack (x:xs:xss) = (x < xs) && (validateStack (xs:xss))
validateStack [] = True
validateStack (x:[]) = True

-- test runner  
testRun :: Int -> PegWorld
testRun size = move (A,B) (createWorld size)
