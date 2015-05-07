public class SameFringeTest {
  /**
   * Construct a tree's leaf with a label
   */
  public static <A> Tree<A> leaf(A label) {
    return new Tree.Leaf<>(label);
  }

  /**
   * Construct a tree's node from a left and right child tree
   */
  public static <A> Tree<A> node(Tree<A> left, Tree<A> right) {
    return new Tree.Node<>(left, right);
  }

  /**
   * Construct a tree, degenerated into a list going down to the right right
   */
  private static Tree<Integer> generateRightishTree(int leafCount) {
    Tree<Integer> t = leaf(leafCount - 1);
    for (int i = leafCount - 1; i-- > 0;) {
      t = node(leaf(i), t);
    }
    return t;
  }

  /**
   * Construct a tree, degenerated into a list going down to the left
   */
  private static Tree<Integer> generateLeftishTree(int leafCount) {
    Tree<Integer> t = leaf(0);
    for (int i = 1; i < leafCount; i++) {
      t = node(t, leaf(i));
    }
    return t;
  }

//      yes = True
//      no  = False
//
//      size :: Int
//      size = 1000
//
//      time :: IO a -> IO a
//      time f = do
//        startTime <- getCurrentTime
//        result <- f
//        endTime <- getCurrentTime
//        let elapsedTime = diffUTCTime endTime startTime in do
//         putStrLn $ " elapsed time: " ++ show elapsedTime
//         return result
//
//      test :: String -> Bool -> Tree Int -> Tree Int -> IO ()
//      test name expected left right = do
//        putStrLn $ " running test: " ++ name
//        time $
//          let actual = sameFringe left right in
//           if expected == actual
//           then do
//             return ()
//           else do
//             putStrLn $ "expected: " ++ show expected
//             putStrLn $ "actual:   " ++ show actual
//             error "test failure"
//        putStrLn ""
//
//      main :: IO ()
//      main = do
//        putStrLn "SameFringe [Haskell]"
//
//        (test "same leaves" yes
//              (Leaf 1)
//              (Leaf 1))
//
//        (test "different leaves" no
//              (Leaf 1)
//              (Leaf 2))
//
//        (test "same trees" yes
//              (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
//              (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))
//
//        (test "rightish/rightish different first" no
//              (Node (Leaf 1) (Node (generateRightishTree size) (Leaf 0)))
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
//
//        (test "leftish/leftish different first" no
//              (Node (Leaf 2) (Node (generateLeftishTree size)  (Leaf 0)))
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
//
//        (test "rightish/leftish different first" no
//              (Node (Leaf 3) (Node (generateRightishTree size) (Leaf 0)))
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
//
//        (test "leftish/rightish different first" no
//              (Node (Leaf 4) (Node (generateLeftishTree size)  (Leaf 0)))
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
//
//        (test "rightish/rightish" yes
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))
//
//        (test "leftish/leftish" yes
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
//        (test "rightish/leftish" yes
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0)))
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0))))
//        (test "leftish/rightish" yes
//              (Node (Leaf 0) (Node (generateLeftishTree size)  (Leaf 0)))
//              (Node (Leaf 0) (Node (generateRightishTree size) (Leaf 0))))

  public static void main(String[] args) {
    System.out.println("SameFringe [Java]");

    /////////////////////////////
    //           //            //
    //    .      //       .    //
    //   / \     //      / \   //
    //  1   .    //    .    3  //
    //     / \   //   / \      //
    //    2   3  //  1   2     //
    //           //            //
    //           //            //
    /////////////////////////////
    // two binary trees with same fringe, namely 1, 2, 3
    System.out.println(SameFringe.sameFringe(node(leaf(1), node(leaf(2), leaf(3))),
                                             node(node(leaf(1), leaf(2)), leaf(3))));

    /////////////////////////////
    //           //            //
    //    .      //       .    //
    //   / \     //      / \   //
    // "a"  .    //    .   "a" //
    //     / \   //   / \      //
    //   "b" "c" // "c" "b"    //
    //           //            //
    //           //            //
    /////////////////////////////
    // two binary trees with different fringe,
    // namely "a", "b", "c" and "c", "b", "a"
    System.out.println(SameFringe.sameFringe(node(leaf("a"), node(leaf("b"), leaf("c"))),
                                             node(node(leaf("c"), leaf("b")), leaf("a"))));
  }
}
