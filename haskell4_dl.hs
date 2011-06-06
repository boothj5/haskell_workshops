lowestNonSpecialCard :: [Int] -> Int
lowestNonSpecialCard = minimum . filter (not . specialCard)

canMove :: [Int] -> [Int] -> [Int] -> Bool
canMove pile hand1 hand2 =  foldl (\acc card -> if validMove card pile then True else acc) False $ hand1 ++ hand2