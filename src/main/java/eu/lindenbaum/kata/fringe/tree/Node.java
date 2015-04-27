package eu.lindenbaum.kata.fringe.tree;

public final class Node<A> implements Tree<A> {
  public final Tree<A> left;
  public final Tree<A> right;

  public Node(Tree<A> left, Tree<A> right) {
    this.left = left;
    this.right = right;
  }
}
