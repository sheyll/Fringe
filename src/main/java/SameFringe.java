/**
 * <p>
 * In 1977, in the pages of the ACM SIGART Newsletter, a debate raged over
 * whether samefringe is the simplest problem that requires multiprocessing or
 * CoRoutines to easily solve... The samefringe problem is this: two binary
 * trees have the same fringe if they have exactly the same leaves reading from
 * left to right.
 * </p>
 * <p>
 * The problem is easy to solve in Haskell, since non-strict evaluation does
 * just as much work as necessary to produce a result.
 * </p>
 * 
 * <pre>
 *   data Tree a = Leaf a | Node (Tree a) (Tree a)
 * 
 *   leaves :: Tree a -> [a]
 *   leaves (Leaf a)   = [a]
 *   leaves (Node l r) = leaves l ++ leaves r
 * 
 *   sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
 *   sameFringe a b    = leaves a == leaves b
 * </pre>
 * <p>
 * (taken from <a href="http://c2.com/cgi/wiki?SameFringeProblem">
 * http://c2.com/cgi/wiki?SameFringeProblem</a>)
 * </p>
 */
public class SameFringe {
  /**
   * Construct an empty list
   */
  public static <A> List<A> nil() {
    return new List.Nil<>();
  }

  /**
   * Construct a list pair
   */
  public static <A> List<A> cons(A head, List<A> tail) {
    return new List.Pair<>(head, tail);
  }

  /**
   * Append two lists
   */
  public static <A> List<A> concat(List<A> l1, List<A> l2) {
    if (l1 instanceof List.Nil) {
      return l2;
    }
    else {
      List.Pair<A> c1 = (List.Pair<A>) l1;
      return new List.Pair<>(c1.head, concat(c1.tail, l2));
    }
  }

  /**
   * Check two lists for equality
   */
  public static <A> boolean same(List<A> l1, List<A> l2) {
    if (l1 instanceof List.Nil) {
      return l2 instanceof List.Nil;
    }
    if (l2 instanceof List.Nil) {
      return l1 instanceof List.Nil;
    }
    else {
      List.Pair<A> c1 = (List.Pair<A>) l1;
      List.Pair<A> c2 = (List.Pair<A>) l2;
      return c1.head.equals(c2.head) && same(c1.tail, c2.tail);
    }
  }

  /**
   * Create a list from a tree, containing the leaf node's labels, from left to
   * right
   */
  public static <A> List<A> leaves(Tree<A> t) {
    if (t instanceof Tree.Leaf) {
      Tree.Leaf<A> l = (Tree.Leaf<A>) t;
      return cons(l.label, nil());
    }
    else {
      Tree.Node<A> n = (Tree.Node<A>) t;
      return concat(leaves(n.left), leaves(n.right));
    }
  }

  /**
   * Check the leaves of two trees for equality
   */
  public static <A> boolean sameFringe(Tree<A> t1, Tree<A> t2) {
    return same(leaves(t1), leaves(t2));
  }
}
