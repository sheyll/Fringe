package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.List.append;
import static eu.lindenbaum.kata.fringe.List.eq;
import static eu.lindenbaum.kata.fringe.List.singleton;

/**
 * Binary tree data structure with leaf node labels of type A.
 */
public interface Tree<A> {
  /**
   * Tree case, representing a leaf with a label.
   */
  public static final class Leaf<A> implements Tree<A> {
    private final A label;

    private Leaf(A label) {
      this.label = label;
    }
  }

  /**
   * Tree case, representing a node with two child trees.
   */
  public static final class Node<A> implements Tree<A> {
    public final Tree<A> left;
    public final Tree<A> right;

    private Node(Tree<A> left, Tree<A> right) {
      this.left = left;
      this.right = right;
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
      return singleton(l.label);
    }
    else {
      Node<A> n = (Node<A>) t;
      return append(leaves(n.left), leaves(n.right));
    }
  }

  /**
   * Check the leaves of two trees for equality
   */
  public static <A> boolean sameFringe(Tree<A> t1, Tree<A> t2) {
    return eq(leaves(t1), leaves(t2));
  }
}
