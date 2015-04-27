package eu.lindenbaum.kata.fringe.tree;

import static eu.lindenbaum.kata.fringe.list.List.append;
import static eu.lindenbaum.kata.fringe.list.List.eq;

import eu.lindenbaum.kata.fringe.list.Cons;
import eu.lindenbaum.kata.fringe.list.List;
import eu.lindenbaum.kata.fringe.list.Nil;

public interface Tree<A> {
  public static <A> List<A> leaves(Tree<A> t) {
    if (t instanceof Leaf) {
      Leaf<A> l = (Leaf<A>) t;
      return new Cons<A>(l.label, new Nil<A>()); // FIXME add cons, ncons to List, make constructor Cons(...) package private 
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
