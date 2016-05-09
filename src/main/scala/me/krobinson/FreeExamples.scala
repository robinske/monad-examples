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

case object PrintEvaluator extends FunctorTransformer[Todo, Option] {
  def apply[A](a: Todo[A]): Option[A] = {
    a match {
      case NewTask(task) =>
        println(s"New task added: $task")
        Some(task)
      case CompleteTask(task) =>
        println(s"Task completed: $task")
        Some(task)
      case GetTasks(default) =>
        println(s"Request to fetch tasks")
        Some(default)
    }
  }
}


object FreeExamples {
  import Todo._

  // make id a monad so we can use it in our `runFree` code
  implicit val idMonad = new Monad[Id] {
    def pure[A](given: A): Id[A] = given
    def flatMap[A, B](given: Id[A])(fn: A => Id[B]): Id[B] = fn(given)
  }

  // make Option a monad so we can use it in our `runFree` code
  implicit val optMonad = new Monad[Option] {
    def pure[A](given: A): Option[A] = Some(given)
    def flatMap[A, B](given: Option[A])(fn: A => Option[B]): Option[B] = given match {
      case Some(o) => fn(o)
      case None    => None
    }
  }

  val freeStrings: Free[Id, String] =
    for {
      a <- Return("chain")
      b <- Return("these")
      c <- Return("together")
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
    FlatMap(
      Suspend(NewTask("Go to scala days")), (a: String) =>
      FlatMap(
        Suspend(NewTask("Write a novel")), (b: String) =>
        FlatMap(
          Suspend(NewTask("Meet Tina Fey")), (c: String) =>
          FlatMap(
            Suspend(CompleteTask("Go to scala days")), (d: String) =>
            Suspend(GetTasks(default = Map.empty))
          )
        )
      )
    )

  def main(args: Array[String]) {
    runFree(todos)(PrintEvaluator)
  }

}