package me.krobinson.monads


// for examples about the laws applying, check out the tests

object IntegerAddition extends Monoid[Int] {

  def append(a: Int, b: Int): Int = a + b

  def identity: Int = 0

}

object IntegerMultiplication extends Monoid[Int] {

  def append(a: Int, b: Int): Int = a * b

  def identity: Int = 1

}

object FunctionComposition /* extends Monoid[_=>_] */ {

  def append[A, B, C](f1: A => B, f2: B => C): A => C =
    (a: A) => f2(f1(a))

  def identity[A]: A => A = { a => a }

}

object StringConcat extends Monoid[String] {

  def append(a: String, b: String): String = a + b

  def identity: String = ""

}

class ListConcat[A] extends Monoid[List[A]] {

  def append(a: List[A], b: List[A]): List[A] = a ++ b

  def identity: List[A] = List.empty[A]

}