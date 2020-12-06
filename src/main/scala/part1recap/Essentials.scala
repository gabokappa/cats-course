package part1recap

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values

  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions

  val theUnit = println("hello scala") // Unit is the equivalent in other languages as "void" basically any side effects

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model a class extend another class but mixin more than one

  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Chomp!")
  }

  // singleton

  object MySingleton // singleton pattern in one line

  // companions

  object Carnivore // this is the companion object of the class Carnivore if a field and method are defined in this object they can be called in classes

  // Generics

  class MyList[A]

  // method notation

  val three= 1 + 2
  val otherThree = 1.+(2)

  // funtional programming

  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // map, flatMap. filter These are higher order functions

  val processedList = List(1,2,3).map(incrementer)
  println(processedList)

  // for=comprehensions
  val checkerboard = List(1,2,3).flatMap(n => List("a", "b", "c").map(c => (n, c))) // this is a tuple containing (1, a)
  val anotherCheckeredboard = for {
    n <- List(1,2,3)
    c <- List("a", "b", "c")
  } yield (n, c)
  println(s"for comprehension $checkerboard")
  println(s"another for compre $anotherCheckeredboard")

  // All the lists created by flatmap are concatenated.
  val aLongerList = List(1,2,3).flatMap(x => List(x, x + 1))
  println(aLongerList)

  // options and try

  val anOption: Option[Int] = Option(3) // something
  val doubledOption: Option[Int] = anOption.map(_ * 2)

  val anAttempt = Try(/* something that might throw */ 42) // it has two sub-types, success or a failure containing the exception
  val aModified: Try[Int] = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures are data structures whose value is computed at some point

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    // this will be evaluated on another threwad
    42
  }

  // wait for completion (async)
  aFuture.onComplete { // the onComplete receives a Try function
    case Success(value) => println(s"the async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // this will contains Future(43) when it completes

  // partial functons
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  // some more advanced stuff

  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {

  }

}
