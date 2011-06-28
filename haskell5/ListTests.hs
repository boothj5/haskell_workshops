import Test.HUnit
import List
import Data.Maybe

emptyList :: List Int
emptyList = Empty

listOfOne :: List Char
listOfOne = Elem '2' Empty

listOfFive :: List String
listOfFive = Elem "Hello" (Elem "There" (Elem "Whats" (Elem "Going" (Elem "On" Empty))))

listOfTen :: List Int
listOfTen = Elem 1 (Elem 2 (Elem 3 (Elem 4 (Elem 5 (Elem 6 (Elem 7 (Elem 8 (Elem 9 (Elem 10 Empty)))))))))

-- Turn List to a primitive []
-- So we can test equality of returned lists
-- as we have not implemented Eq for List
toPrimitive :: List a -> [a]
toPrimitive Empty = []
toPrimitive (Elem x Empty) = x:[]
toPrimitive (Elem x xs) = x:(toPrimitive xs)

-- test listHead
testHeadOnEmpty = 
    TestCase (assertEqual "testHeadOnEmpty"
        True 
        (isNothing (listHead emptyList) ) )

testHeadOnOne = 
    TestCase (assertEqual "testHeadOnOne"
        (Just '2') 
        (listHead listOfOne) )
   
testHeadOnFive = 
    TestCase (assertEqual "testHeadOnFive"
        (Just "Hello") 
        (listHead listOfFive) )
   
-- test listTail
testTailOnEmpty = 
    TestCase (assertEqual "testTailOnEmpty"
        True 
        (isNothing . listTail $ emptyList) ) 
   
testTailOnOne = 
    TestCase (assertEqual "testTailOnOne"
        [] 
        (toPrimitive . fromJust . listTail $ listOfOne) )  
   
testTailOnFive = 
    TestCase (assertEqual "testTailOnFive"
        ["There", "Whats", "Going", "On"] 
        (toPrimitive . fromJust . listTail $ listOfFive) )  

-- test listTail
testSizeOnEmpty = 
    TestCase (assertEqual "testSizeOnEmpty"
        0 
        (listSize emptyList) )

testSizeOnOne = 
    TestCase (assertEqual "testSizeOnOne"
        1 
        (listSize listOfOne) )   
   
testSizeOnFive = 
    TestCase (assertEqual "testSizeOnFive"
        5 
        (listSize listOfFive) )  

-- test listGet
testGetOnEmpty = 
    TestCase (assertEqual "testGetOnEmpty"
        Nothing 
        (listGet 2 emptyList) )

testGetOnOne = 
    TestCase (assertEqual "testGetOnOne"
        (Just '2') 
        (listGet 0 listOfOne) )

testGetOnFiveFirst = 
    TestCase (assertEqual "testGetOnFiveFirst"
        (Just "Hello") 
        (listGet 0 listOfFive) )

testGetOnFiveThird = 
    TestCase (assertEqual "testGetOnFiveThird"
        (Just "Whats") 
        (listGet 2 listOfFive) )

testGetOnFiveFifth = 
    TestCase (assertEqual "testGetOnFiveFifth"
        (Just "On") 
        (listGet 4 listOfFive) )

testGetOnFiveSixth = 
    TestCase (assertEqual "testGetOnFiveSixth"
        Nothing 
        (listGet 5 listOfFive) )

-- test listMap
testMapOnEmpty = 
    TestCase (assertEqual "testMapOnEmpty"
        []
        (toPrimitive $ listMap (+1) emptyList) )

testMapOnFive = 
    TestCase (assertEqual "testMapOnFive"
        ["Hello!!", "There!!", "Whats!!", "Going!!", "On!!"]
        (toPrimitive $ listMap (++"!!") listOfFive) )

testMapOnTen = 
    TestCase (assertEqual "testMapOnTen"
        [(-10), (-20)..(-100)]
        (toPrimitive $ listMap (\x -> negate x * 10) listOfTen) )

-- Suite
tests = TestList [ TestLabel "testHeadOnEmpty" testHeadOnEmpty
                 , TestLabel "testHeadOnOne" testHeadOnOne
                 , TestLabel "testHeadOnFive" testHeadOnFive

                 , TestLabel "testTailOnEmpty" testTailOnEmpty
                 , TestLabel "testTailOnOne" testTailOnOne
                 , TestLabel "testTailOnFive" testTailOnFive
                 
                 , TestLabel "testSizeOnEmpty" testSizeOnEmpty
                 , TestLabel "testSizeOnOne" testSizeOnOne
                 , TestLabel "testSizeOnFive" testSizeOnFive
                 
                 , TestLabel "testGetOnEmpty" testGetOnEmpty
                 , TestLabel "testGetOnOne" testGetOnOne
                 , TestLabel "testGetOnFiveFirst" testGetOnFiveFirst
                 , TestLabel "testGetOnFiveThird" testGetOnFiveThird
                 , TestLabel "testGetOnFiveFifth" testGetOnFiveFifth
                 , TestLabel "testGetOnFiveSixth" testGetOnFiveSixth

                 , TestLabel "testMapOnEmpty" testMapOnEmpty
                 , TestLabel "testMapOnFive" testMapOnFive
                 , TestLabel "testMapOnTen" testMapOnTen
                 ]

main = runTestTT tests
