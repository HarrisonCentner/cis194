-- Submit File
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parse   :: String -> [LogMessage]
insert  :: LogMessage -> MessageTree -> MessageTree
build   :: [LogMessage] -> MessageTree 
inOrder :: MessageTree -> [LogMessage]
isSevere :: LogMessage -> Bool
getString :: LogMessage -> String
whatWentWrong :: [LogMessage] -> [String]


parseMessage s = 
  case words s of
    ("E":sev:time:xs) 
                  -> LogMessage (Error (read sev)) (read time) (unwords xs)
    ("I":time:xs) -> LogMessage Info (read time) (unwords xs)
    ("W":time:xs) -> LogMessage Warning (read time) (unwords xs)
    _             -> Unknown s

-- If the tree is a leaf then do inserting. 
-- Discard if the message is Unknown
insert (Unknown _) tree = tree
insert lg Leaf = Node Leaf lg Leaf
insert lg@(LogMessage _ time _) (Node left curr@(LogMessage _ t _) right)
  | time <= t = Node (insert lg left) curr right
  | time >  t = Node left curr (insert lg right)
insert _ tree = tree;

build arr = foldr insert  Leaf arr

inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

isSevere (LogMessage (Error svr) _ _)
  | svr >= 50 = True
  | otherwise = False
isSevere _ = False

getString (Unknown string) = string
getString (LogMessage _ _ string) = string

whatWentWrong msgs = map getString (filter isSevere msgs)


parse fileString = map parseMessage (lines fileString)

