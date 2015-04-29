package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.Tree.leaf;
import static eu.lindenbaum.kata.fringe.Tree.node;
import static eu.lindenbaum.kata.fringe.Tree.sameFringe;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("static-method")
public class SameFringeTest {
  @Test
  public void testEqualLeaves() {
    Tree<String> t1 = leaf("a");
    Tree<String> t2 = leaf("a");
    assertTrue(sameFringe(t1, t2));
  }

  @Test
  public void testDifferentLeaves() {
    Tree<String> t1 = leaf("a");
    Tree<String> t2 = leaf("b");
    assertFalse(sameFringe(t1, t2));
  }

  @Test
  public void testLeafAndNode() {
    Tree<String> t1 = leaf("a");
    Tree<String> t2 = node(leaf("a"), leaf("b"));
    assertFalse(sameFringe(t1, t2));
  }

  @Test
  public void testEqualNodes() {
    Tree<String> t1 = node(leaf("a"), leaf("b"));
    Tree<String> t2 = node(leaf("a"), leaf("b"));
    assertTrue(sameFringe(t1, t2));
  }

  @Test
  public void testDifferentNodes() {
    Tree<String> t1 = node(leaf("a"), leaf("b"));
    Tree<String> t2 = node(leaf("b"), leaf("c"));
    assertFalse(sameFringe(t1, t2));
  }

  @Test
  public void testEqualTrees() {
    Tree<String> t1 = node(leaf("a"), node(leaf("b"), leaf("c")));
    Tree<String> t2 = node(node(leaf("a"), leaf("b")), leaf("c"));
    assertTrue(sameFringe(t1, t2));
  }

  @Test
  public void testGeneratedTrees_100() {
    Tree<String> t1 = generateLeftishTree(100);
    Tree<String> t2 = generateRightishTree(100);
    assertTrue(sameFringe(t1, t2));
  }

  @Ignore
  @Test
  public void testGeneratedTrees_100_100() {
    Tree<String> t1 = generateLeftishTree(100 * 100);
    Tree<String> t2 = generateRightishTree(100 * 100);
    assertTrue(sameFringe(t1, t2));
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
