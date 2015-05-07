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

  private static Tree<String> generateLeftishTree(int leafCount) {
    Tree<String> t = leaf(String.valueOf(0));
    for (int i = 1; i < leafCount; i++) {
      t = node(t, leaf(String.valueOf(i)));
    }
    return t;
  }

  private static Tree<String> generateRightishTree(int leafCount) {
    Tree<String> t = leaf(String.valueOf(leafCount - 1));
    for (int i = leafCount - 1; i-- > 0;) {
      t = node(leaf(String.valueOf(i)), t);
    }
    return t;
  }

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
