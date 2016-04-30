package me.krobinson.monads

trait Monoid[A] {
  def append(a: A, b: A): A
  def identity: A
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(fn: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] /* with Monoid[_ => M[_]] */ {
  def pure[A](a: A): M[A]
  def flatMap[A, B](a: M[A])(fn: A => M[B]): M[B]

  def map[A, B](a: M[A])(fn: A => B): M[B] = {
    flatMap(a){ b: A => pure(fn(b)) }
  }

  def append[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C] = { a: A =>
    val bs: M[B] = f1(a)
    val cs: M[C] = flatMap(bs) { b: B => f2(b) }
    cs
  }

  def identity[A]: A => M[A] = a => pure(a)
}

object Monad {
  def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
}
