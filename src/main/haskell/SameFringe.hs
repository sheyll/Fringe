module SameFringe ( Tree(..)
                  , sameFringe
                  )
       where

data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a -> [a]
leaves (Leaf a)   = [a]
leaves (Node l r) = leaves l ++ leaves r

sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
sameFringe a b    = leaves a == leaves b
