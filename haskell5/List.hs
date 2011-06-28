module List where

data List a = Empty | Elem a (List a)

instance (Show a) => Show (List a) where
    show Empty          = ""
    show (Elem x Empty) = show x
    show (Elem x xs)    = show x ++ ", " ++ show xs
    
listHead :: List a -> Maybe a
listHead _ = Nothing

listTail :: List a -> Maybe (List a)
listTail _ = Nothing

listSize :: List a -> Int
listSize _ = 0

listGet :: Int -> List a -> Maybe a
listGet _ _ = Nothing 

listMap :: (a -> b) -> List a -> List b
listMap _ _ = Empty
