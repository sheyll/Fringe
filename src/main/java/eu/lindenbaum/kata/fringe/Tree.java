package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.List.append;
import static eu.lindenbaum.kata.fringe.List.eq;
import static eu.lindenbaum.kata.fringe.List.ncons;

public interface Tree<A> {
  public static final class Leaf<A> implements Tree<A> {
    public final A label;

    public Leaf(A label) {
      this.label = label;
    }
  }

  public static final class Node<A> implements Tree<A> {
    public final Tree<A> left;
    public final Tree<A> right;

    public Node(Tree<A> left, Tree<A> right) {
      this.left = left;
      this.right = right;
    }
  }

  public static <A> List<A> leaves(Tree<A> t) {
    if (t instanceof Leaf) {
      Leaf<A> l = (Leaf<A>) t;
      return ncons(l.label);
    }
    else {
      Node<A> n = (Node<A>) t;
      return append(leaves(n.left), leaves(n.right));
    }
  }

  public static <A> boolean sameFringe(Tree<A> t1, Tree<A> t2) {
    return eq(leaves(t1), leaves(t2));
  }
}
