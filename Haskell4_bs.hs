module Haskell4_bs where

specialCard :: Int -> Bool
specialCard = (`elem` [2,7,10])
        
validMove :: Int -> [Int] -> Bool
validMove c  []        = True
validMove c1 (c2:cs)  
    | specialCard c1   = True
    | c2 == invisible    = validMove c1 cs
    | c1 >= c2             = True
    | otherwise           = False
      where invisible = 7

replaceCardWithCard :: [Int] -> Int -> Int -> [Int]
replaceCardWithCard [] _ _ = []
replaceCardWithCard (x:xs) old new
         | old == x = new : xs
         | otherwise = x : replaceCardWithCard xs old new

lowestCard :: [Int] -> Int
-- lowestCard [] = error "Error"
lowestCard (x:xs)
        | [] == xs = x
         | not (specialCard x) && x < low = x
         | otherwise  = low
         where low = lowestCard xs
