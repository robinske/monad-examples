package me.krobinson.monads

import org.scalatest.{FunSpec, Matchers}
import me.krobinson.monads.Free._


class Spec extends FunSpec with Matchers {

  import FreeExamples._

  case class TestEvaluator(var model: Map[String, Boolean]) extends FunctorTransformer[Todo, Id] {
    def apply[A](a: Todo[A]): Id[A] = {
      a match {
        case NewTask(task) =>
          model = model + (task.toString -> false)
          task
        case CompleteTask(task) =>
          model = model + (task.toString -> true)
          task
        case GetTasks(default) =>
          model.asInstanceOf[A]
      }
    }
  }

  case object ActionTestEvaluator extends FunctorTransformer[Todo, Id] {
    var actions: List[Todo[String]] = List.empty
    def apply[A](a: Todo[A]): Id[A] = {
      a match {
        case NewTask(task) =>
          actions = actions :+ NewTask(task.toString)
          task
        case CompleteTask(task) =>
          actions = actions :+ CompleteTask(task.toString)
          task
        case GetTasks(default) =>
          actions = actions :+ GetTasks("")
          default
      }
    }
  }

  describe("Free") {
    it("should evaluate todos") {
      val result = runFree(todos)(TestEvaluator(Map.empty))

      val expected: Map[String, Boolean] =
        Map(
          "Go to scala days" -> true,
          "Write a novel" -> false,
          "Meet Tina Fey" -> false
        )

      result shouldBe expected
      runFree(todos)(TestEvaluator(Map.empty)) shouldBe runFree(todosExpanded)(TestEvaluator(Map.empty))
    }

    it("should evaluate todos with an action evaluator") {
      runFree(todos)(ActionTestEvaluator)

      val expected: List[Todo[String]] =
        List(
          NewTask("Go to scala days"),
          NewTask("Write a novel"),
          NewTask("Meet Tina Fey"),
          CompleteTask("Go to scala days"),
          GetTasks("")
        )

      ActionTestEvaluator.actions shouldBe expected
    }
  }

  describe("#runLoop") {
    it("should evaluate the Free Monad") {
      val expected: Map[String, Boolean] =
        Map(
          "Go to scala days" -> true,
          "Write a novel" -> false,
          "Meet Tina Fey" -> false
        )

      Free.runLoop(todos)(TestEvaluator(Map.empty)) shouldBe expected
    }
  }

  describe("Free") {
    it("should construct a nested datastructure with .flatMap and .map methods") {
      val expected: Free[Id, String] =
        More[Id, String, String](
          Done[Id, String]("chain"), a => More[Id, String, String](
            Done[Id, String]("these"), b => More[Id, String, String](
              Done[Id, String]("together"), c => Done[Id, String](s"$a $b $c")
            )
          )
        )

      Free.run(freeStrings) shouldBe Free.run(expected)
    }
  }

  describe("#run") {
    it("should evaluate the Free Monad") {
      Free.run(freeStrings) shouldBe "chain these together"
    }
  }

  import OptionFunctor._

  describe("OptionFunctor") {
    it("should map the custom option when it is a 'Some'") {
      val mine: Option[String] = Some("foo")
      OptionFunctor.map(mine)(_ + "s") shouldBe Some("foos")
    }

    it("should map the custom option when it is a 'None'") {
      val mine: Option[String] = None
      OptionFunctor.map(mine)(_ + "s") shouldBe None
    }

    it("should follow identity and composition properties") {

      // Identity:
      assert(map(Some("foo"))(identity) == Some("foo"))

      // Composition:
      val f: String => String = s => s + "a"
      val g: String => String = s => s + "l"
      val h: String => String = s => s + "a"
      assert(map(Some("sc"))(f andThen g andThen h) == map(map(map(Some("sc"))(f))(g))(h))
    }
  }

  describe("MonoidExamples") {
    it("should follow Monoid laws for string concatenation") {
      import StringConcat._

      // associativity
      assert(append("foo", append("bar", "baz")) == append(append("foo", "bar"), "baz"))

      // identity
      assert(append("foo", identity) == "foo")
      assert(append(identity, "foo") == "foo")
    }

    it("should follow Monoid laws for integer addition") {
      import IntegerAddition._

      // associativity
      assert(append(2, append(3, 4)) == append(append(2, 3), 4))

      // identity
      assert(append(1, identity) == 1)
      assert(append(identity, 1) == 1)
    }

    it("should follow Monoid laws for integer multiplication") {
      import IntegerMultiplication._

      // associativity
      assert(append(2, append(3, 4)) == append(append(2, 3), 4))

      // identity
      assert(append(1, identity) == 1)
      assert(append(identity, 1) == 1)
    }

    it("should follow Monoid laws for function composition") {
      import FunctionComposition._

      // associativity
      val f = { s: String => s + "a" }
      val g = { s: String => s + "l" }
      val h = { s: String => s + "a" }
      // evaluate the functions, can't compare functions for tests
      assert(append(f, append(g, h))("sc") == append(append(f, g), h)("sc"))

      // identity
      assert(append(f, identity[String])("") == "a")
      assert(append(identity[String], f)("") == "a")
    }

  }
}
