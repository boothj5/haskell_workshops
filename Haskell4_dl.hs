module Haskell4_dl where

specialCard :: Int -> Bool
specialCard c = c `elem` [2,7,10]
 
lowestCard :: [Int] -> Int
lowestCard = minimum . filter (not . specialCard)

validMove :: Int -> [Int] -> Bool
validMove _  []        = True
validMove c1 (c2:cs)  
    | specialCard c1   = True
    | c2 == invisible  = validMove c1 cs
    | c1 >= c2         = True
    | otherwise        = False
      where invisible = 7

canMove :: [Int] -> [Int] -> [Int] -> Bool
canMove pile hand1 hand2 =  foldl (\acc card -> if validMove card pile then True else acc) False $ hand1 ++ hand2
