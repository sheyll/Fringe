package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.Tree.leaf;
import static eu.lindenbaum.kata.fringe.Tree.node;
import static eu.lindenbaum.kata.fringe.Tree.sameFringe;
import static eu.lindenbaum.kata.fringe.Tree.show;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SameFringeTest {
  @Test
  public void test() {
    Tree<String> t1 = node(leaf("a"), node(leaf("b"), leaf("c")));
    Tree<String> t2 = node(node(leaf("a"), leaf("b")), leaf("c"));
    assertTrue(sameFringe(t1, t2));
  }

  @Test
  public void test2() {
    Tree<String> t1 = generateLeftishTree(100);
    Tree<String> t2 = generateRightishTree(100);
    assertTrue(sameFringe(t1, t2));
    System.out.println("leftish  " + show(t1));
    System.out.println("rightish " + show(t2));
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
}
