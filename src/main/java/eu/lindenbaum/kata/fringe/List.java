package eu.lindenbaum.kata.fringe;

/**
 * Recursive list, made of zero or more elements of the same type
 */
public interface List<A> {
  /**
   * List case, representing the empty list
   */
  public static final class Nil<A> implements List<A> {
  }

  /**
   * List case, representing a pair of a head element and a tail list
   */
  public static final class Cons<A> implements List<A> {
    public final A head;
    public final List<A> tail;

    private Cons(A head, List<A> tail) {
      this.head = head;
      this.tail = tail;
    }
  }

  /**
   * Construct a list from elements
   */
  public static <A> List<A> singleton(A a) {
    return new Cons<>(a, new Nil<>());
  }

  /**
   * Append two lists
   */
  public static <A> List<A> append(List<A> l1, List<A> l2) {
    if (l1 instanceof Nil) {
      return l2;
    }
    else {
      Cons<A> c1 = (Cons<A>) l1;
      return new Cons<>(c1.head, append(c1.tail, l2));
    }
  }

  /**
   * Check two lists for equality
   */
  public static <A> boolean eq(List<A> l1, List<A> l2) {
    if (l1 instanceof Nil) {
      return l2 instanceof Nil;
    }
    if (l2 instanceof Nil) {
      return l1 instanceof Nil;
    }
    else {
      Cons<A> c1 = (Cons<A>) l1;
      Cons<A> c2 = (Cons<A>) l2;
      return c1.head.equals(c2.head) && eq(c1.tail, c2.tail);
    }
  }
}
