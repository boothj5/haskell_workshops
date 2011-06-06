replaceCardWithCard :: [Int] -> Int -> Int -> [Int]
replaceCardWithCard [] _ _ = []
replaceCardWithCard (x:xs) c1 c2
    | x == c1   = c2 : xs
    | otherwise = x : replaceCardWithCard xs c1 c2
    
replaceCardWithCard' :: [Int] -> Int -> Int -> [Int]
replaceCardWithCard' cs c1 c2 = map (replaceCardIfMatching c1 c2) cs

replaceCardIfMatching :: Int -> Int -> Int -> Int
replaceCardIfMatching c1 c2 c
    | c == c1 = c2
    | otherwise = c
    
replaceCardWithCard'' :: [Int] -> Int -> Int -> [Int]
replaceCardWithCard'' cs c1 c2 = map p cs
    where p x | x == c1 = c2 | otherwise = x

specialCard :: Int -> Bool
specialCard = (`elem` [2,7,10])
        
lowestCard :: [Int] -> Int
lowestCard [] = error "List of cards is empty"
lowestCard cs = minimum (filter notSpecial cs)
    where notSpecial x = not (specialCard x)
          
validMove :: Int -> [Int] -> Bool
validMove c  []        = True
validMove c1 (c2:cs)  
    | specialCard c1   = True
    | c2 == invisible    = validMove c1 cs
    | c1 >= c2             = True
    | otherwise           = False
      where invisible = 7
      
eitherPlayerCanMove :: [Int] -> [Int] -> [Int] -> Bool
eitherPlayerCanMove h1 h2 p
    | playerCanMove h1 p = True
    | playerCanMove h2 p = True
    | otherwise = False

playerCanMove :: [Int] -> [Int] -> Bool
playerCanMove [] _ = False
playerCanMove (x:xs) p
    | validMove x p = True
    | otherwise = playerCanMove xs p
    
