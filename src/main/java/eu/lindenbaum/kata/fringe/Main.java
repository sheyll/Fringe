package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.Tree.leaf;
import static eu.lindenbaum.kata.fringe.Tree.node;
import static eu.lindenbaum.kata.fringe.Tree.sameFringe;

/**
 * Demonstrate the samefringe problem.
 */
public class Main {
  public static void main(String[] args) {
    System.out.println("Fringe");

    /////////////////////////////
    //           //            //
    //    .      //       .    //
    //   / \     //      / \   //
    //  1   .    //    .    3  //
    //     / \   //   / \      //
    //    2   3  //  1   2     //
    //           //            //
    //           //            //
    /////////////////////////////
    // two binary trees with same fringe, namely 1, 2, 3
    System.out.println(sameFringe(node(leaf(1), node(leaf(2), leaf(3))),
                                  node(node(leaf(1), leaf(2)), leaf(3))));

    /////////////////////////////
    //           //            //
    //    .      //       .    //
    //   / \     //      / \   //
    // "a"  .    //    .   "a" //
    //     / \   //   / \      //
    //   "b" "c" // "c" "b"    //
    //           //            //
    //           //            //
    /////////////////////////////
    // two binary trees with different fringe,
    // namely "a", "b", "c" and "c", "b", "a"
    System.out.println(sameFringe(node(leaf("a"), node(leaf("b"), leaf("c"))),
                                  node(node(leaf("c"), leaf("b")), leaf("a"))));
  }
}
