module Main ( main )
       where

import SameFringe
import Data.Time.Clock

generateRightishTree :: Int -> Tree Int
generateRightishTree size =
  generate 0
  where generate i =
          if i < size
          then
            Node (Leaf i) (generate (i + 1))
          else
            Leaf size

generateLeftishTree :: Int -> Tree Int
generateLeftishTree size =
  if size > 0
  then
    Node (generateLeftishTree (size - 1)) (Leaf size)
  else
    Leaf 0

time :: IO a -> IO a
time f = do
  startTime <- getCurrentTime
  result <- f
  endTime <- getCurrentTime
  let elapsedTime = diffUTCTime endTime startTime in do
   putStrLn $ " elapsed time: " ++ show elapsedTime
   return result

test :: String -> Bool -> Tree Int -> Tree Int -> IO ()
test name expected left right = do
  putStrLn $ " running test: " ++ name
  time $
    let actual = sameFringe left right in
     if expected == actual
     then do
       return ()
     else do
       putStrLn $ "expected: " ++ show expected
       putStrLn $ "actual:   " ++ show actual
       error "test failure"
  putStrLn ""

yes = True
no  = False

size :: Int
size = 1000

main :: IO ()
main = do
  putStrLn "SameFringe [Haskell]"

  (test "same leaves" yes
        (Leaf 1)
        (Leaf 1))

  (test "different leaves" no
        (Leaf 1)
        (Leaf 2))

  (test "same trees" yes
        (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
        (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

  (test "rightish/rightish different first" no
        (Node (Leaf 1) (Node (generateRightishTree size) (Leaf 0)))
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

  (test "leftish/leftish different first" no
        (Node (Leaf 2) (Node (generateLeftishTree size)  (Leaf 0)))
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))

  (test "rightish/leftish different first" no
        (Node (Leaf 3) (Node (generateRightishTree size) (Leaf 0)))
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))

  (test "leftish/rightish different first" no
        (Node (Leaf 4) (Node (generateLeftishTree size)  (Leaf 0)))
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

  (test "rightish/rightish" yes
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

  (test "leftish/leftish" yes
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
  (test "rightish/leftish" yes
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
  (test "leftish/rightish" yes
        (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
        (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
