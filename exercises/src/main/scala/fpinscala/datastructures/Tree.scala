package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}

// Tree.size(Branch(Leaf(1), Leaf(2)))
// Tree.maximum(Branch(Leaf(1), Leaf(2)))
// Tree.depth(Branch(Leaf(11), Branch(Branch(Leaf(1211), Leaf(1212)), Leaf(122))))
// Tree.map(Branch(Leaf(11), Branch(Branch(Leaf(1211), Leaf(1212)), Leaf(122))))(_ * 2)