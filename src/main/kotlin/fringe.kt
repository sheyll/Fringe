package com.github.rusio

abstract class Tree {
  abstract fun fringe(): List<Tree>
}

data class Leaf(val value: Int) : Tree() {
  override fun fringe(): List<Tree> {
    return listOf(this)
  }
}

data class Node(val a: Tree, val b: Tree) : Tree() {
  override fun fringe(): List<Tree> {
    return a.fringe() + b.fringe()
  }
}
