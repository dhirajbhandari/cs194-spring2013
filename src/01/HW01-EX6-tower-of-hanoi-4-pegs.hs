-- Hw 1 Ex:6
-- Tower of Hanoi with 4 pegs
--

data Peg = A | B | C | D deriving (Show, Eq)
type Move = (Peg, Peg)

-- Tower of Hanoi world
data PegWorld = PegWorld 
  {
    pwStackA  :: PegStack
  , pwStackB  :: PegStack
  , pwStackC  :: PegStack
  , pwStackD  :: PegStack
  , pwMoves   :: [Move]
  } deriving (Show, Eq)

moves :: PegWorld -> [Move]
moves w = reverse (pwMoves w)

type PegStack = [Int]

-- move N disks from Peg a to b in the given Pegworld, return resulting PegWorld
move :: Move -> PegWorld -> PegWorld 
move (a,b) w = moveN (a,b,n) w
  where n = length (stackByName w a)

------------------------------------
-- internals
------------------------------------
moveN :: (Peg,Peg,Int) -> PegWorld -> PegWorld
moveN (p1,p2,n) w
  | n == 1    = applyMove (p1, p2) w
  | n == 2    = applyMove (p3, p2) (applyMove (p1,p2) (applyMove (p1, p3) w))
  | otherwise = afterCB
  where
    afterAC = moveN (p1, p3, (n - 2)) w
    afterAD = applyMove (p1, p4) afterAC
    afterAB = applyMove (p1, p2) afterAD
    afterDB = applyMove (p4, p2) afterAB
    afterCB = moveN (p3, p2, (n - 2 )) afterDB
    (p3,p4) = otherPeg p1 p2 w
                 

applyMove :: Move -> PegWorld -> PegWorld
applyMove (x,y) w@(PegWorld s1 s2 s3 s4 moves) = w3
  where
    (n:xs)  = stackByName w x 
    ys      = n:(stackByName w y) 
    w3      = modifyWorld w (x, xs) (y, ys)

modifyWorld :: PegWorld -> (Peg, PegStack) -> (Peg, PegStack) -> PegWorld
modifyWorld w (x, xs) (y,ys) = PegWorld as bs cs ds ((x,y):(pwMoves w))
 where 
  as = if x == A then xs else if y == A then ys else pwStackA w
  bs = if x == B then xs else if y == B then ys else pwStackB w
  cs = if x == C then xs else if y == C then ys else pwStackC w
  ds = if x == D then xs else if y == D then ys else pwStackD w

otherPeg :: Peg -> Peg -> PegWorld -> (Peg,Peg)
otherPeg x y _ = (v,w)
  where
   others = [a|a<- [A,B,C,D], not (a `elem` [x,y])]
   (v:vs) = others
   w      = head vs

stackByName :: PegWorld -> Peg -> [Int]
stackByName w A = pwStackA w
stackByName w B = pwStackB w
stackByName w C = pwStackC w
stackByName w D = pwStackD w


---------------------------------------------------------------------
-- Verification & Testing
---------------------------------------------------------------------

-- convinience Methods
startWorld :: PegWorld 
startWorld = createWorld 2

createWorld :: Int -> PegWorld 
createWorld n = PegWorld [1..n] [] [] [] []


testAll :: (Bool, String)
testAll = foldl joinResults (True, "") allTests
  where 
    test1 = testCase 1 [(A,B)]
    test2 = testCase 2 [(A,C), (A,B), (C,B)]
    test3 = testCase 3 [(A,C),(A,D),(A,B),(D,B),(C,B)]
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
     validateWorld (PegWorld a b c d _) = all validateStack [a, b,c,d] 
     w1 = applyMove x startWorld
  in (validateWorld w1) && (validateMoves w1 xs)


validateStack :: PegStack -> Bool
validateStack (x:xs:xss) = (x < xs) && (validateStack (xs:xss))
validateStack [] = True
validateStack (x:[]) = True

-- test runner  
testRun :: Int -> PegWorld
testRun size = move (A,B) (createWorld size)
