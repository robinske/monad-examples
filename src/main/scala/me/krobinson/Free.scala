package me.krobinson.monads

sealed trait Free[F[_], A] { self =>
  def map[B](fn: A => B): Free[F, B] =
    flatMap(a => Return(fn(a)))

  def flatMap[B](fn: A => Free[F, B]): Free[F, B] =
    FlatMap(self, (a: A) => fn(a))
}

case class Return[F[_], A](given: A) extends Free[F, A]
case class FlatMap[F[_], A, B](given: Free[F, A], fn: A => Free[F, B]) extends Free[F, B]

object Free {
  sealed trait Context[A]

  @annotation.tailrec
  def run(f: Free[Context, String]): String = f match {
    case Return(s)                         => s
    case FlatMap(FlatMap(given, fn1), fn2) => run(given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2))))
    case FlatMap(Return(given), fn)        => run(fn(given))
  }

  def runLoop(c: Free[Context, String]): String = {
    var eval: Free[Context, String] = c

    while (true) {
      eval match {
        case Return(s) =>
          return s
        case FlatMap(FlatMap(given, fn1), fn2) =>
          eval = given.flatMap(s1 => fn1(s1).flatMap(s2 => fn2(s2)))
        case FlatMap(Return(s), fn) =>
          eval = fn(s)
      }
    }

    throw new AssertionError("Unreachable")
  }
}
