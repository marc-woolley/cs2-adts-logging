-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Log
import LogInstances

-- import everything *except* `main` from LogAnalysis
import LogAnalysis hiding (main)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "parseMessage Info"
    ( parseMessage "I 6 Completed armadillo processing" @?=
      LogMessage Info 6 "Completed armadillo processing" )
    -- If you don't have a function called praseMessage, change the
    -- test to match your code.

    -- Add at least 3 more test cases for 'parseMessage', including
    -- one with an error, one with a warning, and one with an Unknown
   , testCase "parseMessage Warning"
    ( parseMessage "W 6 the armadillo is processing" @?=
      LogMessage Warning 6 "the armadillo is processing" )

  ,  testCase "parseMessage Error Int"
    ( parseMessage "E 45 6 armadillos can't process" @?=
      LogMessage (Error 45) 6 "armadillos can't process" )

    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
  

  , testCase "Data type Info test for checkmessagemessagetype"
    ( checkmessagemessagetype ["I","help","uoy","mum m8"]
  
      @?= "help")



  , testCase "Data type Warning test for checkmessagemessagetype"
    ( checkmessagemessagetype ["W","help","uoy","mum m8"]
      @?= "help")



  , testCase "Data type Error test for checkmessagemessagetype"
    ( checkmessagemessagetype ["E","2","uoy","mum m8"]
      @?= "uoy")
  
    -- Add at least 3 more tests for MessageType parsing in isolation.

    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.
    
    --Test the time stamp contions

    , testCase "dropthisamount testing otherwise"
    ( dropthisamount ["I","2","uoy","mum m8"]
      @?= 2)

    , testCase "dropthisamount testing Error b output"
    ( dropthisamount ["E","2","uoy","mum m8"]
      @?= 3)


    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

--I think you need to test every function with atleast every way it could ever be used in the program
--What I mean by this is if a function has three uses in a program or rather three possible outputs
-- Then you need three test


    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.
    , testCase "insert unknown"
    ( insert (Unknown "the armadillo is processing") Leaf 
      @?= Leaf)
    
     , testCase "insert into tree with no elements"
    ( insert (LogMessage Warning 6 "the armadillo is processing") (Node Leaf (LogMessage Warning 6 "the armadillo is processing Leaf") Leaf)
    @?= Node Leaf (LogMessage Warning 6 "the armadillo is processing") Leaf)

       , testCase "insert into tree with elements"
    ( insert (LogMessage Warning 4 "the armadillo is processing") (Node (Node Leaf (LogMessage Warning 6 "the armadillo is processing") Leaf ) (LogMessage Warning 70 "the armadillo is processing Leaf") (Node Leaf (LogMessage Info 4 "the hype is real") Leaf) )
    @?=  Node (Node Leaf (LogMessage Warning 4 "the armadillo is processing") Leaf) (LogMessage Warning 70 "the armadillo is processing Leaf") (Node Leaf (LogMessage Info 4 "the hype is real") Leaf)) 

    -- Next week we'll have the computer write more tests, to help us         
    -- be more confident that we've tested all the tricky bits and
    -- edge cases.  There are also tools to make sure that our tests
    -- actually run every line of our code (called "coverage"), but we
    -- won't learn those this year.

    -- Write tests for 'inOrder'.  Remember that the input tree is
    -- meant to already be sorted, so it's fine to only test such
    -- inputs.  You may want to reuse MessageTrees from the tests on
    -- 'insert' above.  You may even want to move them elsewhere in
    -- the file and give them names, to more easiely reuse them.
   ,testCase " test tree with many elements"
   ( inOrder (Node (Node Leaf (LogMessage Warning 4 "the armadillo is processing") Leaf) (LogMessage Warning 70 "the armadillo is processing Leaf") (Node Leaf (LogMessage Info 80 "the hype is real") Leaf))
    @?= [LogMessage Warning 4 "the armadillo is processing", LogMessage Warning 70 "the armadillo is processing Leaf", LogMessage Info 80 "the hype is real"] )

   ,testCase " test tree with onr element"
   ( inOrder (Node Leaf (LogMessage Warning 6 "the armadillo is processing Leaf") Leaf)
    @?= [LogMessage Warning 6 "the armadillo is processing Leaf"])

  ,testCase "test tree with no elements"
  ( inOrder Leaf
   @?= [])

    , testProperty "build sorted"
    (\msgList -> isSorted (inOrder (build msgList)))
  
  

    -- show :: Int -> String
    -- gives the String representation of an Int
    -- Use show to test your code to parse Ints
    
    -- Write a function that takes a MessageType, and makes a String
    -- with the same format as the log file:
    -- stringMessageType :: MessageType -> String
    -- Use this to test your code that parses MessageType

    -- Make another function that makes a String from a whole LogMessage
    -- stringLogMessage :: LogMessage -> String
    -- Use it to test parseMessage
    -- write a function that checks if error message types is true or false



  ]

stringMessageType:: MessageType -> String
stringMessageType Info = "I" 
stringMessageType Warning = "W" 
stringMessageType (Error a) = "E " ++ show a


 --doesStringMessageTypeWork :: MessageType -> String
 --doesStringMessageTypeWork a =  == 



stringLogmessageType :: LogMessage -> String
stringLogmessageType (LogMessage a d c) = unwords([(stringMessageType a) ,(show d), c])



main = defaultMain tests
