 
data Fruit = Apple | Grapefruit | Lychee | Banana
    deriving Show
    
opinion :: Fruit -> String
opinion Lychee     = "Ugh"
opinion Grapefruit = "Super yum"
opinion _          = "Yum"

data Person = Person String String Int
    deriving Show
    
firstName :: Person -> String
firstName (Person f _ _) = f

secondName :: Person -> String
secondName (Person _ s _) = s

age :: Person -> Int
age (Person _ _ a) = a

type Name = String

data Person = Person { firstName  :: Name
                     , secondName :: Name
                     , age        :: Int 
                     } 
                     
instance Show Person where
    show p = secondName p ++ ", " ++ firstName p 
             ++ ": " ++ show (age p) 
             ++ " years old."
                   
returnMaybe 0 = Nothing
returnMaybe x = Just x


    

