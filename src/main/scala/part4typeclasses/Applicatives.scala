package part4typeclasses

object Applicatives {

  // Applicatives = Functors + pure method the ability to wrap a normal value into a "wrapped" value

  import cats.Applicative
  import cats.instances.list._

  // Fetching the implicit applicative of list
  val listApplicative: Applicative[List] = Applicative[List]
  val aList = listApplicative.pure(2) // this will return a List(2)

import cats.instances.option._ // to fetch the implicit Applicative[Option]

val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // This returns Some(2)

  // Applicatives are useful because they are functors they can run the map method and wrapping a normal type into a wrapped type.
  // Section the part2 of this course in the functors section. Fucntors are useful for generalising APIs.

  // pure extension method of applicative. This is the same method as the pure used in the Monads in section 2.

  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // this needs to have the type of applicative in scope in order to work
  val aSweetOption = 2.pure[Option]

  // Monads extend applicatives aas they inheret the pure method. Applicatives extend Functors
  // Applicatives are rarely used by themselves, they are used as part of the stronger type which is the Monad. The exception to that is Validated

  import cats.data.Validated

  type ErrorOr[T] = Validated[List[String], T]
  val aValidValue: ErrorOr[Int] = Validated.valid(43) // its as if you called "pure" on 43 and turned it into the Validated wrapper type
  val aModifiedValidated: ErrorOr[Int] = aValidValue.map(_ + 1) // this is a map

  val validatedApplicative = Applicative[ErrorOr] // this constructs an Applicative of our made up type where we can run the pure in the validated type and map
  // can be used for Functors

  // TODO: thought experiment - define the following method (can't be done)
  // below T is the tuple (A, B)
// def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ???
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = { // return a wrapper of a tuple
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }
// An applicative can be a semigroupal in the pressence of the ap method. The ap method takes this signature ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]. They can implement product from semigroupal

  def main(args: Array[String]): Unit = {

  }

}
