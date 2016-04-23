package me.krobinson.monads

import org.scalatest.{FunSpec, Matchers}
import me.krobinson.monads.Free._


object FreeExamples {
  import Free._

  val free: Free[Id, String] =
    for {
      a <- Done("chain")
      b <- Done("these")
      c <- Done("together")
    } yield s"$a $b $c"

  sealed trait Todo[A]
  case class NewTask[A](task: A) extends Todo[A]
  case class CompleteTask[A](task: A) extends Todo[A]
  case class GetTasks[A](default: A) extends Todo[A]

  def newTask[A](task: A): Free[Todo, A] = Suspend(NewTask(task))
  def completeTask[A](task: A): Free[Todo, A] = Suspend(CompleteTask(task))
  def getTasks[A](default: A): Free[Todo, A] = Suspend(GetTasks(default))


  case object ProductionEvaluator extends (Todo ~> Id) {
    def apply[A](a: Todo[A]): Id[A] = {
      var results: List[A] = List.empty
      a match {
        case NewTask(task) =>
          // database write
          task
        case CompleteTask(task) =>
          // database write
          task
        case GetTasks(default) =>
          // database read
          default
      }
    }
  }

}

class Spec extends FunSpec with Matchers {

  import FreeExamples._

  describe("Free") {
    it("should evaluate todos") {

      val todos: Free[Todo, List[String]] =
        for {
          _    <- newTask("Go to scala days")
          _    <- newTask("Write a novel")
          _    <- newTask("Meet Tina Fey")
          _    <- completeTask("Go to scala days")
          tsks <- getTasks(default = List.empty[String])
        } yield tsks

      val expected: Map[String, Boolean] =
        Map(
          "Go to scala days" -> true,
          "Write a novel" -> false,
          "Meet Tina Fey" -> false
        )

      implicit object id extends Monad[Id] {
        override def pure[A](given: A): Id[A] =
          given

        override def map[A, B](given: Id[A])(fn: A => B): Id[B] =
          fn(given)

        override def flatMap[A, B](given: Id[A])(fn: A => Id[B]): Id[B] =
          fn(given)
      }

      case object TestEvaluator extends (Todo ~> Id) {
        var model: Map[String, Boolean] = Map.empty
        def apply[A](a: Todo[A]): Id[A] = {
          a match {
            case NewTask(task) =>
              model = model + (task.toString -> false)
              task
            case CompleteTask(task) =>
              model = model + (task.toString -> true)
              task
            case GetTasks(default) =>
              default
          }
        }
      }

      val result = runWithInterpreter(todos)(TestEvaluator)
      TestEvaluator.model shouldBe expected
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
      def identity[A](a: A): A = a
      assert(map(Some("foo"))(identity) == Some("foo"))

      // Composition:
      val f: String => String = s => s + "a"
      val g: String => String = s => s + "l"
      val h: String => String = s => s + "a"
      assert(map(Some("sc"))(f andThen g andThen h) == map(map(map(Some("sc"))(f))(g))(h))
    }
  }
}
