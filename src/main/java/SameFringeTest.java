public class SameFringeTest {
  /**
   * Construct a tree's leaf with a label
   */
  public static <A> Tree<A> Leaf(A label) {
    return new Tree.Leaf<>(label);
  }

  /**
   * Construct a tree's node from a left and right child tree
   */
  public static <A> Tree<A> Node(Tree<A> left, Tree<A> right) {
    return new Tree.Node<>(left, right);
  }

  /**
   * Construct a tree, degenerated into a list going down to the right right
   */
  private static Tree<Integer> generateRightishTree(int leafCount) {
    Tree<Integer> t = Leaf(leafCount - 1);
    for (int i = leafCount - 1; i-- > 0;) {
      t = Node(Leaf(i), t);
    }
    return t;
  }

  /**
   * Construct a tree, degenerated into a list going down to the left
   */
  private static Tree<Integer> generateLeftishTree(int leafCount) {
    Tree<Integer> t = Leaf(0);
    for (int i = 1; i < leafCount; i++) {
      t = Node(t, Leaf(i));
    }
    return t;
  }

  private static void time(Runnable f) {
    long startTime = System.currentTimeMillis();
    f.run();
    long endTime = System.currentTimeMillis();
    long elapsedTime = endTime - startTime;
    System.out.println(" elapsed time: " + elapsedTime / 1000.0);

  }

  private static void test(String name, boolean expected, Tree<Integer> left, Tree<Integer> right) {
    System.out.println(" running test: " + name);
    time(() -> {
      boolean actual = SameFringe.sameFringe(left, right);
      if (expected == actual) {
        return;
      }
      else {
        System.out.println(" expected: " + expected);
        System.out.println(" actual:   " + actual);
        throw new Error("test failure");
      }
    });
    System.out.println();
  }

  private static final boolean yes = true;
  private static final boolean no = false;

  private static final int size = 1000;

  public static void main(String[] args) {
    System.out.println("SameFringe [Java]");

    test("same leaves", yes, //
         Leaf(1),
         Leaf(1));

    test("different leaves", no, //
         Leaf(1),
         Leaf(2));

    test("same trees", yes, //
         Node(Leaf(1), Node(Leaf(2), Leaf(3))),
         Node(Node(Leaf(1), Leaf(2)), Leaf(3)));

    test("rightish/rightish different first", no, //
         Node(Leaf(1), Node(generateRightishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))));

    test("leftish/leftish different first", no, //
         Node(Leaf(2), Node(generateLeftishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));

    test("rightish/leftish different first", no, //
         Node(Leaf(3), Node(generateRightishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));

    test("leftish/rightish different first", no, //
         Node(Leaf(4), Node(generateLeftishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))));

    test("rightish/rightish", yes, //
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))));

    test("leftish/leftish", yes, //
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));

    test("rightish/leftish", yes, //
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));

    test("leftish/rightish", yes, //
         Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))),
         Node(Leaf(0), Node(generateRightishTree(size), Leaf(0))));

    for (int i = 0; i <= 100; i++) {
      int size = 1000 * i;
      test("leftish/leftish " + size, yes, //
           Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))),
           Node(Leaf(0), Node(generateLeftishTree(size), Leaf(0))));
    }
  }
}
