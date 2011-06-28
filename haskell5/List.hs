module List where

data List a = Empty | Elem a (List a)

instance (Show a) => Show (List a) where
    show Empty          = ""
    show (Elem x Empty) = show x
    show (Elem x xs)    = show x ++ ", " ++ show xs
    
listHead :: List a -> Maybe a
listHead _ = error "Function not implemented"

listTail :: List a -> Maybe (List a)
listTail _ = error "Function not implemented"

listSize :: List a -> Int
listSize _ = error "Function not implemented"

listGet :: Int -> List a -> Maybe a
listGet _ _ = error "Function not implemented" 

listMap :: (a -> b) -> List a -> List b
listMap _ _ = error "Function not implemented"
