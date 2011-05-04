numIsSeven :: Int -> String
numIsSeven 7 = "Hit"
numIsSeven x = show x ++ " is not 7"



















printNum :: Int -> String  
printNum 1 = "One"  
printNum 2 = "Two"  
printNum 3 = "Three"  
printNum 4 = "Four"  
printNum 5 = "Five"  
printNum x = show x ++ " is not between 1 and 5" 















printNum2 :: Int -> String  
printNum2 1 = "One"  
printNum2 2 = "Two"  
printNum2 3 = "Three"  
printNum2 4 = "Four"  
printNum2 5 = "Five"  
printNum2 _ = "It's not between 1 and 5" 















factorial :: Integer -> Integer  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  



















newHead :: [a] -> a
newHead (first:rest) = first




















newHead2 :: [a] -> a
newHead2 (first:_) = first




















newHead3 :: [a] -> a
newHead3 [] = error "Can't call newHead on empty list"
newHead3 (first:_) = first



















newHead4 :: [a] -> Maybe a
newHead4 [] = Nothing
newHead4 (first:_) = Just first



















first :: (a, b, c) -> a
first (x, _, _) = x




















second :: (a, b, c) -> b
second (_, y, _) = y




















third :: (a, b, c) -> c
third (_, _, z) = z




















guardFunc :: Float -> String
guardFunc n 
    | n <= 10.25 = show n ++ " <= 10.25"
    | n <= 20.75 = "10.25 < " ++ show n ++ " <= 20.75"
    | otherwise  = show n ++ " > 20.75"
    
    
    
    
    
    
    
    
    
    







validMove :: Int -> [Int] -> Bool
validMove x  []        = True
validMove x1 (x2:xs)  
    | specialNumber x1 = True
    | x2 == invisible  = validMove x1 xs
    | x1 >= x2         = True
    | otherwise        = False
      where specialNumber x = x `elem` [2, 7, 10]
	    invisible = 7
    
    
    










addSqToDb :: Int -> Int -> Int
addSqToDb x y = let sqx = x * x
                    dby = y + y
                in sqx + dby
                
                
                
                
                
                
                
                
                









addSqToDb2 :: Int -> Int -> Int
addSqToDb2 x y = sqx + dby
    where sqx = x * x
          dby = y + y


















guardFunc2 :: Float -> String
guardFunc2 n =
    if n <= 10.25 
        then show n ++ " <= 10.25"
        else if n <= 20.75 
            then "10.25 < " ++ show n ++ " <= 20.75"
            else show n ++ " > 20.75"















printNum3 :: Int -> String  
printNum3 x = 
    case x of
        1 -> "One"  
        2 -> "Two"  
        3 -> "Three"  
        4 -> "Four"  
        5 -> "Five"  
        _ -> "It's not between 1 and 5" 