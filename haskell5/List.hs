module List where

data List a = Empty | Elem a (List a)

instance (Show a) => Show (List a) where
    show Empty          = ""
    show (Elem x Empty) = show x
    show (Elem x xs)    = show x ++ ", " ++ show xs