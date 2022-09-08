{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("I":time:msg') -> LogMessage Info (read time) (unwords msg')
    ("W":time:msg') -> LogMessage Warning (read time) (unwords msg')
    ("E":level:time:msg') -> LogMessage (Error (read level)) (read time) (unwords msg')
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse text = [parseMessage line | line <- (lines text)]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgt = msgt
insert lgmsg Leaf = Node Leaf lgmsg Leaf
insert lgmsg@(LogMessage _ time _) (Node left lgmsg'@(LogMessage _ time' _) right)
    | time > time' = Node left lgmsg' (insert lgmsg right)
    | otherwise = Node (insert lgmsg left) lgmsg' right

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logmsg right) = (inOrder left) ++ [logmsg] ++ (inOrder right)

isSeriousError :: LogMessage -> Bool
isSeriousError (LogMessage (Error level) _ _)
    | level >= 50 = True
    | otherwise = False
isSeriousError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = 
    [
        msg |
        (LogMessage _ _ msg) <- inOrder . build . (filter isSeriousError) $ logs
    ]