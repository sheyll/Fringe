data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

leaves :: Tree a -> [a]
leaves (Leaf a)          = [a]
leaves (Node left right) = leaves left ++ leaves right

sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
sameFringe a b = leaves a == leaves b

generateRightishTree :: Int -> Tree Int
generateRightishTree size =
  generate 0
  where generate current =
          if current < size
          then
            Node (Leaf current) (generate (current + 1))
          else
            Leaf size

generateLeftishTree :: Int -> Tree Int
generateLeftishTree size =
  if size > 0
  then
    Node (generateLeftishTree (size - 1)) (Leaf size)
  else
    Leaf 0

size :: Int
size = 10000

main :: IO()
main = do
  putStrLn (show (sameFringe (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))) (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))))
  putStrLn (show (sameFringe (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))) (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))))
  putStrLn (show (sameFringe (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))) (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))))
  putStrLn (show (sameFringe (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))) (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))))
