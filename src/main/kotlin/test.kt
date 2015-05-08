package com.github.rusio

import org.junit.Assert
import org.junit.Test

public class FringeTest {
  Test fun test() {
    val root1 = Node(
        Node(
            Leaf(1),
            Leaf(2)),
        Leaf(3))
    val root2 = Node(
        Leaf(1),
        Node(
            Leaf(2),
            Leaf(3)))
    Assert.assertEquals(root1.fringe(), root2.fringe())
  }
}

