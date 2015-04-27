package eu.lindenbaum.kata.fringe.tree;

public final class Leaf<A> implements Tree<A> {
  public final A label;

  public Leaf(A label) {
    this.label = label;
  }
}
