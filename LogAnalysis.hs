module LogAnalysis where

import Log

parseMessagehelper::String->[String]
parseMessagehelper a = words a

parseMessagehelper2:: [String]->String
parseMessagehelper2 a = unwords a 

charToString :: Char -> String
charToString c = [c]

errormessagetypes::[String]->MessageType 
errormessagetypes (a:rest) | a!!0 == 'I' = Info
					| a!!0 == 'W' = Warning
					| a!!0 == 'E' && b<100 = Error b where
						b = read (rest!!0)

checkmessagemessagetype::[String]->String
checkmessagemessagetype (c:cs) | errormessagetypes(c:cs) == Info = cs!!0
							   | errormessagetypes(c:cs) == Warning = cs!!0   
							   | otherwise = cs!!1

dropthisamount::[String]->Int 
dropthisamount (c:cs) = case errormessagetypes(c:cs) of 
	Error b  -> 3 
	otherwise ->2 

parseMessage :: String -> LogMessage
parseMessage a = LogMessage (errormessagetypes(untranslatederror)) (read (checkmessagemessagetype(untranslatederror))) (parseMessagehelper2 (drop (dropthisamount(untranslatederror)) untranslatederror)) where
	untranslatederror = (parseMessagehelper a) 

parse :: String -> [LogMessage]
parse a = map (parseMessage) (lines a)

findtimestamp::LogMessage->Int
findtimestamp (LogMessage _ time _ ) = time
insert :: LogMessage -> MessageTree -> MessageTree
insert a (Leaf) = Leaf 
insert a (Node Leaf b Leaf)= Node Leaf a Leaf
insert a (Node leftbranch b rightbranch) | findtimestamp(a)>findtimestamp(b) =leftbranch
                                         | findtimestamp(a)<findtimestamp(b) =rightbranch

build :: [LogMessage] -> MessageTree
build a = foldr insert (Leaf) a



inOrder :: MessageTree -> [LogMessage]
inOrder a@(Leaf) = [] 
inOrder a@(Node Leaf b Leaf) = [b]
inOrder a@(Node leftbranch b rightbranch) = inOrder leftbranch


