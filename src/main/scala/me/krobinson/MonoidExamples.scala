package me.krobinson.monads

object IntegerAddition extends Monoid[Int] {
  def append(a: Int, b: Int): Int = a + b
  def identity: Int = 0
  // Associativity: 2 + (3 + 4) == (2 + 3) + 4
  // Identity: (1 + 0) == (0 + 1) == 1
}

object FunctionComposition /* extends Monoid[_ => _] */ {
  def append[A, B, C](a: A => B, b: B => C): A => C = a.andThen(b)
  def identity[A]: A => A = a => a
  // Associativity: (f.andThen(g.andThen(h)))(x) == ((f.andThen(g)).andThen(h))(x)
  // Identity: identitity(f(x)) == f(identity(x)) == f(x)
}

object StringConcat extends Monoid[String] {
  def append(a: String, b: String): String = a + b
  def identity: String = ""
  // Associativity: "foo" + ("bar" + "baz") == ("foo" + "bar") + "baz"
  // Identity: ("foo" + "") == ("" + "foo") == "foo"
}

class ListConcat[A] extends Monoid[List[A]] {
  def append(a: List[A], b: List[A]): List[A] = a ++ b
  def identity: List[A] = List.empty[A]
  // Associativity: List(1,2,3) ++ (List(4,5,6) ++ List(7,8,9)) == (List(1,2,3) ++ List(4,5,6)) ++ List(7,8,9)
  // Identity: (List(1,2,3) ++ Nil) == (Nil ++ List(1,2,3)) == List(1,2,3)
}