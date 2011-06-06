module Haskell4 where

square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x + x

addSquares :: Int -> Int -> Int
addSquares x y = square x + square y


addSquare3ToSquare :: Int -> Int
addSquare3ToSquare x = addSquares 3 x

addSquare3ToSquareAgain :: Int -> Int
addSquare3ToSquareAgain = addSquares 3

add2 :: Int -> Int
add2 = (+2)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


mymap :: (a -> b) -> [a] -> [b]  
mymap _ [] = []  
mymap f (x:xs) = f x : mymap f xs 


myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []  
myfilter p (x:xs)   
    | p x       = x : myfilter p xs  
    | otherwise = myfilter p xs
    
    
-- Card excersises
 
-- | Given a hand, replace one card in it with another one passed
replaceCardWithCard :: [Int] -> Int -> Int -> [Int]
replaceCardWithCard hand old new = map (\c -> if old == c then new else c) hand

 -- | Special cards
specialCard :: Int -> Bool
specialCard c = c `elem` [2,7,10]
 
-- | Return the players lowest card from their hand
lowestCard :: [Int] -> Int
lowestCard hand = minimum $ filter (not . specialCard) hand

validMove :: Int -> [Int] -> Bool
validMove _  []        = True
validMove c1 (c2:cs)  
    | specialCard c1   = True
    | c2 == invisible  = validMove c1 cs
    | c1 >= c2         = True
    | otherwise        = False
      where invisible = 7
	    

canMove :: [Int] -> [Int] -> [Int] -> Bool
canMove _ _ [] = True
canMove hand faceup pile 
    | (not . null) hand   = canMoveWithCards hand pile 
    | (not . null) faceup = canMoveWithCards faceup pile
    | otherwise           = False
 
canMoveWithCards :: [Int] -> [Int] -> Bool
canMoveWithCards _ []       = True
canMoveWithCards cards pile = 
  foldl (\can card -> (can || validMove card pile)) False cards
