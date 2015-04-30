{-# LANGUAGE DeriveGeneric #-}

module Main where

import Test.QuickCheck


data Tree a = Leaf a | Node (Tree a) (Tree a)
     deriving (Eq, Show)


leaves :: Tree a -> [a]
leaves (Leaf x)   = [x]
leaves (Node l r) = leaves l ++ leaves r

sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
sameFringe l r = leaves l == leaves r


reflexive :: Tree Bool -> Bool
reflexive x = sameFringe x x

symmetric :: Tree Bool -> Tree Bool -> Bool
symmetric l r = sameFringe l r == sameFringe r l

isomorph :: Tree Bool -> Tree Bool -> Bool
isomorph l r = (l == r) == sameFringe l r


main :: IO ()
main = quickCheckWith args allProperties
  where
    allProperties = conjoin [ property reflexive
                            , property symmetric
                            , property isomorph
                            ]

    args = stdArgs { maxSize = 10000, maxSuccess = 100 }


instance (Arbitrary a, Eq a) => Arbitrary (Tree a) where
   arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = Leaf <$> arbitrary
arbTree n = Node <$> arbTree (div n 2) <*> arbTree (div n 2)
