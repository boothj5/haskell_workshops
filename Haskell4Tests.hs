import Test.HUnit
import Haskell4
-- import Haskell4_dl
-- import Haskell4_ks
-- import Haskell4_bs 

-- So we can pick the test function
replaceFunction = replaceCardWithCard

--Test replaceCardWithCard
testReplace1 = 
    TestCase (assertEqual
        "TestReplace1"
        [] (replaceFunction [] 2 3) )

testReplace2 = 
    TestCase (assertEqual
        "TestReplace2"
        [1,5,7] (replaceFunction [1,5,7] 2 3) )

testReplace3 = 
    TestCase (assertEqual
        "TestReplace3"
        [1,3,7] (replaceFunction [1,2,7] 2 3) )

testReplace4 = 
    TestCase (assertEqual
        "TestReplace4"
        [1] (replaceFunction [4] 4 1) )

-- Test lowsetCard
testLowest1 = 
    TestCase (assertEqual
        "TestLowest1"
        3 (lowestCard [4,3,5,6]))

testLowest2 = 
    TestCase (assertEqual
        "TestLowest2"
        3 (lowestCard [3,14]))

testLowest3 = 
    TestCase (assertEqual
        "TestLowest3"
        3 (lowestCard [3,2,4]))

testLowest4 = 
    TestCase (assertEqual
        "TestLowest4"
        8 (lowestCard [7,8,9]))

testLowest5 = 
    TestCase (assertEqual
        "TestLowest5"
        12 (lowestCard [7,14,12,13,2]))
        

-- Test canMove
testCanMove1 = 
    TestCase (assertBool
        "TestCanMove1"
        (canMove [2,3,4,5] [6,5,3,5] []) )

testCanMove2 = 
    TestCase (assertBool
        "TestCanMove2"
        (canMove [3,4,5] [6,5,3,5] [3,9]) )

testCanMove3 = 
    TestCase (assertBool
        "TestCanMove3"
        (canMove [] [6,5,3,5] [3,9]) )
   
testCanMove4 = 
    TestCase (assertBool
        "TestCanMove4"
        (canMove [2] [] [9,12]) )
   
testCanMove5 = 
    TestCase (assertBool
        "TestCanMove5"
        (canMove [] [3,2] [9,12]) )

testCanMove6 = 
    TestCase (assertBool
        "TestCanMove6"
        (canMove [3,5,3] [] [7,2]) )

testCanMove7 = 
    TestCase (assertBool
        "TestCanMove7"
        (canMove [3,5,3] [] [7,7,2]) )

testCanMove8 = 
    TestCase (assertBool
        "TestCanMove8"
        (canMove [3,5,3] [] [7,7,7,2]) )

testCanMove9 = 
    TestCase (assertBool
        "TestCanMove9"
        (canMove [3,5,3] [] [7,7,7,7,2]) )

testCanMove10 = 
    TestCase (assertBool
        "TestCanMove10"
        (canMove [4] [3] [4,6]) )

testCanMove11 = 
    TestCase (assertBool
        "TestCanMove11"
        (not $ canMove [3,5,3] [] [7,7,7,7,6]) )

testCanMove12 = 
    TestCase (assertBool
        "TestCanMove12"
        (not $ canMove [] [5] [7,9]) )

testCanMove13 = 
    TestCase (assertBool
        "TestCanMove13"
        (canMove [10,3] [5,6] [12,13,14]) )

testCanMove14 = 
    TestCase (assertBool
        "TestCanMove14"
        (canMove [] [6,2] [8,9,2]) )

testCanMove15 = 
    TestCase (assertBool
        "TestCanMove15"
        (canMove [12,7] [6,2] [14,8,8]) )

testCanMove16 = 
    TestCase (assertBool
        "TestCanMove16"
        (not $ canMove [12] [7,10,2,13] [13,10]) )

testCanMove17 = 
    TestCase (assertBool
        "TestCanMove17"
        (not $ canMove [4,5,6] [10] [8,8]) )

-- Suite
tests = TestList [ TestLabel "TestLowest1" testLowest1
                 , TestLabel "TestLowest2" testLowest2 
                 , TestLabel "TestLowest3" testLowest3 
                 , TestLabel "TestLowest4" testLowest4 
                 , TestLabel "TestLowest5" testLowest5 
                 , TestLabel "TestReplace1" testReplace1
                 , TestLabel "TestReplace2" testReplace2
                 , TestLabel "TestReplace3" testReplace3
                 , TestLabel "TestReplace4" testReplace4
                 , TestLabel "TestCanMove1" testCanMove1
                 , TestLabel "TestCanMove2" testCanMove2
                 , TestLabel "TestCanMove3" testCanMove3
                 , TestLabel "TestCanMove4" testCanMove4
                 , TestLabel "TestCanMove5" testCanMove5
                 , TestLabel "TestCanMove6" testCanMove6
                 , TestLabel "TestCanMove7" testCanMove7
                 , TestLabel "TestCanMove8" testCanMove8
                 , TestLabel "TestCanMove9" testCanMove9
                 , TestLabel "TestCanMove10" testCanMove10
                 , TestLabel "TestCanMove11" testCanMove11
                 , TestLabel "TestCanMove12" testCanMove12
                 , TestLabel "TestCanMove13" testCanMove13
                 , TestLabel "TestCanMove14" testCanMove14
                 , TestLabel "TestCanMove15" testCanMove15
                 , TestLabel "TestCanMove16" testCanMove16
                 , TestLabel "TestCanMove17" testCanMove17
                 ]

main = runTestTT tests
