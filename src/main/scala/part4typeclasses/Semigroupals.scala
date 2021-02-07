package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals { // is a type class

  trait MySemigroupal[F[+_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(123), Some("a string")) // Some(123, "a string")
  val aNoneTuples = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture: Future[(String, Int)] = Semigroupal[Future].product(Future("meaning of life"), Future(42)) // Future("the meaning of life", 42)

  import cats.instances.list._ // This iascatually importing a Monad[List]
  val aTupledList = Semigroupal[List].product(List(1,2), List("a", "b"))

  // TODO implemet product with Monads so not the semigroupal way
  import cats.Monad
  import cats.syntax.functor._ // this is for map
  import cats.syntax.flatMap._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
  }

  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

//  trait MyMonad[M[_]] extends MySemigroupal[M] {
//    def pure[A](value: A): M[A] // formalising the ability to turn a value into an M(value)
//    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
//    // TODO implement map
//    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
//
//// we can define the product method through flatMap and map
//    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
//      flatMap(fa)(a => map(fb)(b => (a, b)))
//  }

  // TODO MONADS EXTEND SEMIGROUPALS

  // TODO WHY ARE SEMIGROUPALS IMPORTANT? MONAD LAWS ARE THERE TO IMPOSE SEQUENCING OF OPERATIONS BUT SEMIGROUPALS doesn't imp[ose sequencing
  // VALIDATED is a good example of semigroupals without following the monad loaws

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal =Semigroupal[ErrorsOr] // This willimplement product of two validated instances in terms of independently combining error type or valid type in terms of their semigroup
  // this requires the implicit semigroup of a List, which we have applied in the import cats.instances.list._

  val invalidsCombination: ErrorsOr[(Nothing, Nothing)] = validatedSemigroupal.product( // printing will show the combination of error messages through validated
    Validated.invalid(List("this is wrong", "another thing wrong")),
    Validated.invalid(List("This is not right"))
  )

  //
  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // here we're combining two left containign eithers. THE PRODUCT METHOD WILL BE IMPLEMENTED IN TERMS OF map/f;latMap
    Left(List("this is wrong", "another thing wrong")),
    Left(List("This is not right")) // This last error will not be propagated in println. THE FLATMAP METHOD ON EITHER SHORT CIRCUITS THE EVALUATION OF THE SECOND EITHER
  ) // we lose track of errors because of the monad law. The associativity law.

  // The Associativity law m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)

    /*
    Semigroupal are a higher-kinded class which can tuple elements. The fundamental method of a semigroupal is the product method

    - Monads extend semigroupal because the product method of semigroupals can be implemented in terms of map and flatmap
    - Some semigroupals are useful when they are not Monad, a use case is validated because the lack of sequencing of operaitons
    - DONT CONFUSE Semigroup vs Semigroupal. Semigroup has the combine methid |+| with Semigroupal which has the product method which is the tupling

     */
  }

  def main(args: Array[String]): Unit = {
    println(invalidsCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))

  }
}
