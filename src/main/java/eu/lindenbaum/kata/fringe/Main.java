package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.Tree.leaf;
import static eu.lindenbaum.kata.fringe.Tree.node;
import static eu.lindenbaum.kata.fringe.Tree.sameFringe;

public class Main {
  public static void main(String[] args) {
    Tree<String> t1 = node(leaf("a"), node(leaf("b"), leaf("c")));

    Tree<String> t2 = node(node(leaf("a"), leaf("b")), leaf("c"));

    System.out.println(sameFringe(t1, t2));
  }
}
