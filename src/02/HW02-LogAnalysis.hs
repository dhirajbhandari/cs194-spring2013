{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

---------------------------------------------------
-- ex1
---------------------------------------------------
parse :: String -> [LogMessage]
parse fContents = map parseMessage (lines fContents)

parseMessage :: String -> LogMessage
parseMessage str = case parsedMsg of
      Nothing     -> Unknown str
      Just (m, _) -> LogMessage m timestamp (unwords msg)
  where 
    parsedMsg = parseMessageType str
    Just(_, (ts:msg)) = parsedMsg
    timestamp = parseTimeStamp ts


parseMessageType :: String -> Maybe (MessageType, [String])
parseMessageType input
  | code == "I"                      = Just (Info, rest)
  | code == "W"                      = Just (Warning, rest) 
  | code == "E" && (length rest > 0) = Just ((Error errCode), afterCode)
  | otherwise                        = Nothing
  where
      w                   = words input
      code                = head w
      rest                = tail w
      (codeStr:afterCode) = rest
      errCode             = read codeStr::Int

{- -- this cant be done without Monad as the current Prelude.read throws exception
parseErrorMessageType :: String -> Maybe (MessageType, [String])
parseErrorMessageType str = 
  if hasCode then Just (Error code,rest) else Nothing
  where
    (codeStr:rest) = splitAtChar ' ' str
    code           = read codeStr::Int
    hasCode        = code /= Nothing 
  
-}


splitAtChar :: Char -> String -> (String,String)
splitAtChar c str = break (\x-> x == c) str

parseTimeStamp :: String -> Int
parseTimeStamp str = read str::Int

-- test parseMessage

testParseMessage :: Bool
testParseMessage =
  let input           = "E 2 562 help help"
      expectedMessage = LogMessage (Error 2) 562 "help help"
      actualMessage   = parseMessage input
   in actualMessage == expectedMessage

---------------------------------------------------
-- Ex2
---------------------------------------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf   
insert msg (Node lt mt rt)
  | tsmsg < tsmt = Node (insert msg lt) mt rt
  | otherwise    = Node lt mt (insert msg rt)
  where 
    tsmsg = getTimeStamp msg
    tsmt  = getTimeStamp mt

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts
getTimeStamp _ = error "Unknown timestamp"

---------------------------------------------------
-- Ex3
---------------------------------------------------
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

---------------------------------------------------
-- Exercise 4
---------------------------------------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = (inOrder lt) ++ (msg:(inOrder rt))

---------------------------------------------------
-- Ex5
---------------------------------------------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  let
    errMsgs = filter (\x -> (getSeverity x) >= 50) msgs
    mt      = build errMsgs
  in
   map (\(LogMessage _ _ msg) -> msg) (inOrder mt)


filterBySeverity :: [LogMessage] -> Int -> [LogMessage]
filterBySeverity msgs severity = filter (\x -> (getSeverity x) >= severity) msgs

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error s) _ _) = s
getSeverity _ = 0
