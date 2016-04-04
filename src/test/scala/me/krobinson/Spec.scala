package me.krobinson.monads

import org.scalatest.{FunSpec, Matchers}
import me.krobinson.monads.Free._

class Spec extends FunSpec with Matchers {

  val free: Free[Context, String] =
    Return[Context, String]("chain").flatMap { a =>
      Return[Context, String]("these").flatMap { b =>
        Return[Context, String]("together").map { c =>
          s"$a $b $c"
        }
      }
    }

  describe("Free") {
    it("should construct a nested datastructure with .flatMap and .map methods") {
      val expected: Free[Context, String] =
        FlatMap[Context, String, String](
          Return[Context, String]("chain"), a => FlatMap[Context, String, String](
            Return[Context, String]("these"), b => FlatMap[Context, String, String](
              Return[Context, String]("together"), c => Return[Context, String](s"$a $b $c")
            )
          )
        )

      Free.run(free) shouldBe Free.run(expected)
    }
  }

  describe("#run") {
    it("should evaluate the Free Monad") {
      Free.run(free) shouldBe "chain these together"
    }
  }

  describe("#runLoop") {
    it("should evaluate the Free Monad") {
      Free.runLoop(free) shouldBe "chain these together"
    }
  }
}