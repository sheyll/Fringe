/**
 * Binary tree data structure with leaf node labels of type A.
 */
public interface Tree<A> {
  /**
   * Tree case, representing a leaf with a label.
   */
  public static final class Leaf<A> implements Tree<A> {
    public final A label;

    public Leaf(A label) {
      this.label = label;
    }
  }

  /**
   * Tree case, representing a node with two child trees.
   */
  public static final class Node<A> implements Tree<A> {
    public final Tree<A> left;
    public final Tree<A> right;

    public Node(Tree<A> left, Tree<A> right) {
      this.left = left;
      this.right = right;
    }
  }
}
