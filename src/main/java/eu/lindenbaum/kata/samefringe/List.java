package eu.lindenbaum.kata.samefringe;

/**
 * Recursive list, made of zero or more elements of the type A.
 */
public interface List<A> {
  /**
   * List case, representing the empty list.
   */
  public static final class Nil<A> implements List<A> {
  }

  /**
   * List case, representing a pair of a head element and a tail list.
   */
  public static final class Pair<A> implements List<A> {
    public final A head;
    public final List<A> tail;

    public Pair(A head, List<A> tail) {
      this.head = head;
      this.tail = tail;
    }
  }
}
