/**
 * 
 */
package eu.lindenbaum.kata.fringe;

/**
 * @author timo
 */
public class Main {

  /**
   * @param args
   */
  public static void main(String[] args) {
    System.out.println("Fringe");
  }
}
// http://c2.com/cgi/wiki?SameFringeProblem
//
//data Tree a = Leaf a | Node (Tree a) (Tree a)
//
//leaves :: Tree a -> [a]
//leaves (Node left right) = leaves left ++ leaves right
//leaves (Leaf a) = [a]
//
//sameFringe :: (Eq a) => Tree a -> Tree a -> Bool
//sameFringe a b = leaves a == leaves b
