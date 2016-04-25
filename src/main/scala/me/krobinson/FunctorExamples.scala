package me.krobinson.monads


sealed trait Option[+A]
case class Some[A](a: A) extends Option[A]
case object None extends Option[Nothing]

object OptionFunctor extends Functor[Option] {

  def pure[A](a: A): Option[A] = Some(a)

  def identity[A]: A => A = (a: A) => a

  def flatMap[A, B](a: Option[A])(fn: A => Option[B]): Option[B] =
    a match {
      case Some(something) => fn(something)
      case None            => None
    }

  def map[A, B](a: Option[A])(fn: A => B): Option[B] =
    flatMap(a) { b: A => pure(fn(b)) }
}