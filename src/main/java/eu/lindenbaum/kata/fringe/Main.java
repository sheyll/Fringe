/**
 *
 */
package eu.lindenbaum.kata.fringe;

import static eu.lindenbaum.kata.fringe.Tree.sameFringe;

import eu.lindenbaum.kata.fringe.Tree.Leaf;
import eu.lindenbaum.kata.fringe.Tree.Node;

/**
 * @author timo
 */
public class Main {

  /**
   * @param args
   */
  public static void main(String[] args) {
    Tree<String> t1 = new Node<>(new Leaf<>("a"), new Node<>(new Leaf<>("b"), new Leaf<>("c")));
    Tree<String> t2 = new Node<>(new Node<>(new Leaf<>("a"), new Leaf<>("b")), new Leaf<>("c"));
    System.out.println(sameFringe(t1, t2));
  }
}

//  http://c2.com/cgi/wiki?SameFringeProblem
//
//  data Tree a = Leaf a | Node (Tree a) (Tree a)
//
//  leaves :: Tree a -> [a]
//  leaves (Node left right) = leaves left ++ leaves right
//  leaves (Leaf a) = [a]
//
//  sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
//  sameFringe a b = leaves a == leaves b
