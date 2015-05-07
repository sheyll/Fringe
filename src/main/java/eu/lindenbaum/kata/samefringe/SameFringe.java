package eu.lindenbaum.kata.samefringe;

import eu.lindenbaum.kata.samefringe.List.Nil;
import eu.lindenbaum.kata.samefringe.List.Pair;
import eu.lindenbaum.kata.samefringe.Tree.Leaf;
import eu.lindenbaum.kata.samefringe.Tree.Node;

/**
 * Demonstrate the samefringe problem.
 */
public class SameFringe {
  /**
   * Construct an empty list
   */
  public static <A> List<A> nil() {
    return new Nil<>();
  }

  /**
   * Construct a list pair
   */
  public static <A> List<A> cons(A head, List<A> tail) {
    return new Pair<>(head, tail);
  }

  /**
   * Append two lists
   */
  public static <A> List<A> concat(List<A> l1, List<A> l2) {
    if (l1 instanceof Nil) {
      return l2;
    }
    else {
      Pair<A> c1 = (Pair<A>) l1;
      return new Pair<>(c1.head, concat(c1.tail, l2));
    }
  }

  /**
   * Check two lists for equality
   */
  public static <A> boolean same(List<A> l1, List<A> l2) {
    if (l1 instanceof Nil) {
      return l2 instanceof Nil;
    }
    if (l2 instanceof Nil) {
      return l1 instanceof Nil;
    }
    else {
      Pair<A> c1 = (Pair<A>) l1;
      Pair<A> c2 = (Pair<A>) l2;
      return c1.head.equals(c2.head) && same(c1.tail, c2.tail);
    }
  }

  /**
   * Construct a tree's leaf with a label
   */
  public static <A> Tree<A> leaf(A label) {
    return new Leaf<>(label);
  }

  /**
   * Construct a tree's node from a left and right child tree
   */
  public static <A> Tree<A> node(Tree<A> left, Tree<A> right) {
    return new Node<>(left, right);
  }

  /**
   * Create a list from a tree, containing the leaf node's labels, from left to
   * right
   */
  public static <A> List<A> leaves(Tree<A> t) {
    if (t instanceof Leaf) {
      Leaf<A> l = (Leaf<A>) t;
      return cons(l.label, nil());
    }
    else {
      Node<A> n = (Node<A>) t;
      return concat(leaves(n.left), leaves(n.right));
    }
  }

  /**
   * Check the leaves of two trees for equality
   */
  public static <A> boolean sameFringe(Tree<A> t1, Tree<A> t2) {
    return same(leaves(t1), leaves(t2));
  }

  public static void main(String[] args) {
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
    System.out.println(sameFringe(node(leaf(1), node(leaf(2), leaf(3))),
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
    System.out.println(sameFringe(node(leaf("a"), node(leaf("b"), leaf("c"))),
                                  node(node(leaf("c"), leaf("b")), leaf("a"))));
  }
}
