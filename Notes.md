# Data Structures

foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B
  f(a[0], foldRight(as[1:], z)(f))

Scala Lib
  scala> List(1,2,3).foldLeft(5)((acc, el) => {println(acc + " " + el); acc + el})
  5 1
  6 2
  8 3
  res11: Int = 11

  scala> List(1,2,3).foldRight(5)((acc, el) => {println(el + " " + acc); acc + el})
  5 3
  8 2
  10 1
  res10: Int = 11


foldRight (1, 2, 3), 0, +
  (1 + (2 + (3 + 0)))

foldLeft (1, 2, 3), 0, +
  (((0 + 1) + 2) + 3)




TODO
why
  scala> List.foldRight(Cons(1, Cons(2, Cons(3, Nil))), 5) { (a, b) => println(a + " " + b); a + b}
  3 5
  2 8
  1 10
  res1: Int = 11

  scala> List.foldRight_rec(Cons(1, Cons(2, Cons(3, Nil))), 5) { (a, b) => println(a + " " + b); a + b}
  5 3
  8 2
  10 1
  res2: Int = 11

## covariance
trait List[+A]
// A is covariant parameter of List
// meaning List[Dog] is considered a subtype of List[Animal], given Dog is a subtype of Animal.

trait List[A] 
// List is invariant in that type parameter

// as Nil extends List[Nothing]
// and Nothing is a subtype of all types
// covariance in parameter A results in List[Nothing] being a subtype List[Int]

## parameter type: B >: A
B must be equal to or a supertype of A