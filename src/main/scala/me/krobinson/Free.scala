package me.krobinson.monads


sealed trait Free[F[_], A] { self =>
  def flatMap[B](fn: A => Free[F, B]): Free[F, B] =
    More(self, (a: A) => fn(a))

  def map[B](fn: A => B): Free[F, B] =
    flatMap(a => Done(fn(a)))
}

case class Done[F[_], A](given: A) extends Free[F, A]
case class Suspend[F[_], A](fn: F[A]) extends Free[F, A]
case class More[F[_], A, B](free: Free[F, A], fn: A => Free[F, B]) extends Free[F, B]

object Free {

  trait NaturalTransformation[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
  type Id[A] = A

  // basic interpreter for the string concatenation example
  @annotation.tailrec
  def run[F[_], A](f: Free[F, A]): A = f match {
    case Done(s)                     => s
    case More(More(given, fn1), fn2) => run(given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2))))
    case More(Done(given), fn)       => run(fn(given))
  }

  def runWithInterpreter[F[_], G[_], A](f: Free[F, A])(transform: F ~> G)(implicit monad: Monad[G]): G[A] = {
    @annotation.tailrec
    def tailThis(free: Free[F, A]): Free[F, A] = free match {
      case More(More(fr, fn1), fn2) => tailThis(fr.flatMap(a1 => fn1(a1).flatMap(a2 => fn2(a2))))
      case More(Done(a), fn)        => tailThis(fn(a))
      case _                        => free
    }

    tailThis(f) match {
      case Done(a)                  => monad.pure(a)
      case Suspend(fa)              => transform(fa)
      case More(Suspend(fa), fn)    => monad.flatMap(transform(fa))(a => runWithInterpreter(fn(a))(transform))
      case _                        => throw new AssertionError("Unreachable")
    }
  }

  def runLoop[A](c: Free[Id, A]): A = {
    var eval: Free[Id, A] = c

    while (true) {
      eval match {
        case Done(a) =>
          return a
        case More(More(given, fn1), fn2) =>
          eval = given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2)))
        case More(Done(s), fn) =>
          eval = fn(s)
      }
    }

    throw new AssertionError("Unreachable")
  }
}
