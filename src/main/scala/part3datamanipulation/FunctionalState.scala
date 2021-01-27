package part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  type MyState[S, A] = S => (S, A) // S is the type for what we call state. And A is the desired result of a single computation. State is a wrapper over a single function

  import cats.data.State // state will allow you to wrap a function and carry out a single computation

  val countAndSay: State[Int, String]  = { // so the State of the system is type Int and the output of the computation is a String
    State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  }

  val (eleven, counted10) = countAndSay.run(10).value // run here returns an instance of Eval so need to evaluated by calling .value which returns the tupl;e with the next state and the output value

  // These state data structures can be composed so we can expressed interative computations without using mutations
  // state = 'iterative' computations

  // BAd scala
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondCompuation = s"Mutiplied with 5, obtained $a"

  // TODO here the above is expressed in pure FP and with states

  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")) // this is a state data structure that wraps a function. In this case the function is (s:Int => (s, "Some string"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  // TODO 1: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
  }

  val gabrielsCart: State[ShoppingCart, Double] = for { // A composite state transition
    _ <- addToCart("guitar", 500) // each one of these is a state transition. If this was supplied with an initial shopping cart, that shopping cart would be modified by all these additions to the cart
    _ <- addToCart("guitar strings", 20)
    total <- addToCart("cable", 10)
  } yield total

  // TODO 2: return a State data structure that, when run, will not chanfge the state but will issue the value of f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))
  // return a State data structure that, when run returns the value of that state and makes no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))
  // return a state data structure that when run returns Unit and sets the state to that value

  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ())) // takes an initial value of type A. The new state will be the value argument of the method, and it would output unit

  // return a Stat data structure that, when run will return a Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), ()))
 // all of the above methods are implemented in the companion object of State

  import cats.data.State._
  val program: State[Int, (Int, Int, Int)] = for { // when you supply program with an initial state, the rest of the transformations happen in sequence. It looks like  sequential programmw but using immutable values
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 42)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c) // This is computatinal maths. An imperative program reduced to FP

  def main(args: Array[String]): Unit = {
println(compositeTransformation.run(10).value) // can see the end state is taken from the transformation, its modification and then the second modification of state
    println(compositeTransformation2.run(10).value)
    println(compositeFunc(10))
    println(gabrielsCart.run(ShoppingCart(List(), 0)).value) // value obtains the state and the total after all these transformations
    println(gabrielsCart.run(ShoppingCart(List("first in the list"), 50)).value)
    println(program.run(10).value)

  }
}
