package me.krobinson.monads

import me.krobinson.monads.Free._

sealed trait Todo[A]
case class NewTask[A](task: A) extends Todo[A]
case class CompleteTask[A](task: A) extends Todo[A]
case class GetTasks[A](default: A) extends Todo[A]

object Todo {
  def newTask[A](task: A): Free[Todo, A] = Suspend(NewTask(task))
  def completeTask[A](task: A): Free[Todo, A] = Suspend(CompleteTask(task))
  def getTasks[A](default: A): Free[Todo, A] = Suspend(GetTasks(default))
}

case object ProductionEvaluator extends FunctorTransformer[Todo, Option] {
  def apply[A](a: Todo[A]): Option[A] = {
    a match {
      case NewTask(task) =>
        // database write
        Some(task)
      case CompleteTask(task) =>
        // database write
        Some(task)
      case GetTasks(default) =>
        // database read
        Some(default)
    }
  }
}


object FreeExamples {
  import Todo._

  // make id a monad so we can use it in our `runFree` code
  implicit object id extends Monad[Id] {
    def pure[A](given: A): Id[A] = given
    def flatMap[A, B](given: Id[A])(fn: A => Id[B]): Id[B] = fn(given)
  }

  val freeStrings: Free[Id, String] =
    for {
      a <- Done("chain")
      b <- Done("these")
      c <- Done("together")
    } yield s"$a $b $c"

  val todos: Free[Todo, Map[String, Boolean]] =
    for {
      _    <- newTask("Go to scala days")
      _    <- newTask("Write a novel")
      _    <- newTask("Meet Tina Fey")
      _    <- completeTask("Go to scala days")
      tsks <- getTasks(default = Map.empty[String, Boolean])
    } yield tsks

  val todosExpanded: Free[Todo, Map[String, Boolean]] =
    More(
      Suspend(NewTask("Go to scala days")), (a: String) =>
      More(
        Suspend(NewTask("Write a novel")), (b: String) =>
        More(
          Suspend(NewTask("Meet Tina Fey")), (c: String) =>
          More(
            Suspend(CompleteTask("Go to scala days")), (d: String) =>
            Suspend(GetTasks(default = Map.empty))
          )
        )
      )
    )

}