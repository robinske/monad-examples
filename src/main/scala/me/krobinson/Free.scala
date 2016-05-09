package me.krobinson.monads


sealed trait Free[F[_], A] { self =>
  def flatMap[B](fn: A => Free[F, B]): Free[F, B] =
    FlatMap(self, (a: A) => fn(a))

  def pure[T](a: T): Free[F, T] = Return(a)

  def map[B](fn: A => B): Free[F, B] =
    flatMap(a => pure(fn(a)))
}

case class Return[F[_], A](given: A) extends Free[F, A]
case class Suspend[F[_], A](fn: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](free: Free[F, A], fn: A => Free[F, B]) extends Free[F, B]

object Free {

  // AKA Natural Transformation
  trait FunctorTransformer[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = FunctorTransformer[F, G]
  type Id[A] = A

  // basic interpreter for the string concatenation example
  @annotation.tailrec
  def run[F[_], A](f: Free[F, A]): A = f match {
    case Return(s)                     => s
    case FlatMap(FlatMap(given, fn1), fn2) => run(given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2))))
    case FlatMap(Return(given), fn)       => run(fn(given))
  }

  def runFree[F[_], G[_]: Monad, A](f: Free[F, A])(transform: FunctorTransformer[F, G]): G[A] = {
    @annotation.tailrec
    def tailThis(free: Free[F, A]): Free[F, A] = free match {
      case FlatMap(FlatMap(fr, fn1), fn2) => tailThis(fr.flatMap(a1 => fn1(a1).flatMap(a2 => fn2(a2))))
      case FlatMap(Return(a), fn)         => tailThis(fn(a))
      case _                              => free
    }

    val G = Monad[G] // uses implicit objects in constructor

    tailThis(f) match {
      case Return(a)                => G.pure(a)
      case Suspend(fa)              => transform(fa)
      case FlatMap(Suspend(fa), fn) => G.flatMap(transform(fa)){ a => runFree(fn(a))(transform) }
      case _                        => throw new AssertionError("Unreachable")
    }
  }

  def runLoop[F[_], G[_], A](f: Free[F, A])(transform: FunctorTransformer[F, G])(implicit monad: Monad[G]): G[A] = {
    var eval: Free[F, A] = f

    while (true) {
      eval match {
        case Return(a) =>
          return monad.pure(a)
        case Suspend(fa) =>
          return transform(fa)
        case FlatMap(Suspend(fa), fn) =>
          return monad.flatMap(transform(fa))(a => runLoop(fn(a))(transform))
        case FlatMap(FlatMap(given, fn1), fn2) =>
          eval = given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2)))
        case FlatMap(Return(s), fn) =>
          eval = fn(s)
      }
    }

    throw new AssertionError("Unreachable")
  }
}
