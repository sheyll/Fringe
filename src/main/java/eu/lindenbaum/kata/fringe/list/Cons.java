package eu.lindenbaum.kata.fringe.list;

public final class Cons<A> implements List<A> {
  public final A head;
  public final List<A> tail;

  public Cons(A head, List<A> tail) {
    this.head = head;
    this.tail = tail;
  }
}
