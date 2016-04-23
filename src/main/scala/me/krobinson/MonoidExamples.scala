package me.krobinson.monads

object IntegerAddition extends Monoid[Int] {

  def append(a: Int, b: Int): Int = a + b

  def identity: Int = 0

  /**
    * Associativity:
    *    2 + (3 + 4) == (2 + 3) + 4
    *
    * Identity:
    *    (1 + 0) == (0 + 1) == 1
    */
}

object IntegerMultiplication extends Monoid[Int] {

  def append(a: Int, b: Int): Int = a * b

  def identity: Int = 1

  /**
    * Associativity:
    *    2 * (3 * 4) == (2 * 3) * 4
    *
    * Identity:
    *    (5 * 1) == (1 * 5) == 5
    */
}

object FunctionComposition /* extends Monoid[_=>_] */ {

  def append[A, B, C](f1: A => B, f2: B => C): A => C =
    f1.andThen(f2)

  def identity[A]: A => A = { a => a }
  /**
    * Associativity:
    *   (f.andThen(g.andThen(h)))(x) ==
    *   ((f.andThen(g)).andThen(h))(x)
    *
    * Identity:
    *   identitity(f(x)) ==
    *   f(identity(x)) == f(x)
    */
}

object StringConcat extends Monoid[String] {

  def append(a: String, b: String): String = a + b

  def identity: String = ""

  /**
    * Associativity:
    *    "foo" + ("bar" + "baz") ==
    *    ("foo" + "bar") + "baz"
    * Identity:
    *    ("foo" + "") ==
    *    ("" + "foo") == "foo"
    */
}

class ListConcat[A] extends Monoid[List[A]] {
  def append(a: List[A], b: List[A]): List[A] = a ++ b
  def identity: List[A] = List.empty[A]

  /**
    * Associativity:
    *   List(1,2) ++ (List(3,4) ++ List(5,6)) ==
    *   (List(1,2) ++ List(3,4)) ++ List(5,6)
    *
    * Identity:
    *   (List(1,2,3) ++ Nil) == (Nil ++ List(1,2,3)) == List(1,2,3)
    */
}