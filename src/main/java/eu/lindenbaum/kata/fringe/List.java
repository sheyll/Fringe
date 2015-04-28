package eu.lindenbaum.kata.fringe;

public interface List<A> {
  public static final class Nil<A> implements List<A> {
  }

  public static final class Cons<A> implements List<A> {
    public final A head;
    public final List<A> tail;

    public Cons(A head, List<A> tail) {
      this.head = head;
      this.tail = tail;
    }
  }

  public static <A> List<A> append(List<A> l1, List<A> l2) {
    if (l1 instanceof Nil) {
      return l2;
    }
    else {
      Cons<A> c1 = (Cons<A>) l1;
      return new Cons<>(c1.head, append(c1.tail, l2));
    }
  }

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
