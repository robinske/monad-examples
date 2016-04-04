package me.krobinson.monads

trait Monoid[A] {
  def append(a: A, b: A): A
  def identity: A

  /*
   * Such that:
   * Associativity property: `append(a, append(b,c)) == append(append(a,b),c)`
   * Identity property: `append(a, identity) == append(identity, a) == a`
   */
}

trait Functor[F[_]] {
  def map[A, B](a: F[A])(fn: A => B): F[B]
  // Identity: map(fa)(identity) == fa
  // Composition: map(fa)(f andThen g) == map(map(fa)(f))(g)
}

trait Monad[M[_]] { // extends Monoid[_ => M[_]]
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

  // And the laws apply!
  // Associativity: flatMap(pure(a), x => flatMap(f(x), g)) == flatMap(flatMap(pure(a), f), g)
  // Identity: flatMap(pure(a), f) == flatMap(f(x), pure) == f(x)
}