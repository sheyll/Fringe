module SameFringe ( Tree(..)
                  , sameFringe
                  )
       where

data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a -> [a]
leaves (Leaf a)          = [a]
leaves (Node left right) = leaves left ++ leaves right

sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
sameFringe a b = leaves a == leaves b
