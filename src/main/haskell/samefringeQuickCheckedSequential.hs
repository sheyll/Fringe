-- Compile with: ghc -O2 samefringeQuickCheckedSequential.hs -rtsopts -threaded -eventlog
-- For parallelism debug output execute with: ./samefringeQuickCheckedSequential +RTS -N2 -l -M1024m
-- then run: threadscope samefringeQuickCheckedSequential.eventlog
module Main where

import Test.QuickCheck


-- Program/Test parameters:
maxTestTreeSize :: Int
maxTestTreeSize = 20000

-- The program:

main :: IO ()
main = check (reflexive .&&. symmetric .&&. isomorph)

-- The tree:

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- The code under test:

leaves :: (Eq a) => Tree a -> [a]
leaves (Leaf x)   = [x]
leaves (Node l r) = leaves l ++ leaves r

sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
sameFringe l r = leaves l == leaves r

-- The properties that the test asserts:

type TestTree = Tree Bool

reflexive :: TestTree -> Bool
reflexive x = sameFringe x x

symmetric :: TestTree -> TestTree -> Bool
symmetric l r = sameFringe l r == sameFringe r l

isomorph :: TestTree -> TestTree -> Bool
isomorph l r = (l == r) == sameFringe l r

-- quickcheck instance

instance (Arbitrary a, Eq a) => Arbitrary (Tree a) where
  arbitrary = sized arbTree
    where
      arbTree 0 = Leaf <$> arbitrary
      arbTree n = Node <$> arbTree (div n 2)
                       <*> arbTree (div n 2)

-- how quickcheck is invoked:
check :: Property -> IO ()
check = quickCheckWith $ stdArgs
  { maxSuccess = maxTestTreeSize `div` 10
  , maxSize = maxTestTreeSize
  }
