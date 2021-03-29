package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // here we import an implicit MonadError for either
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // This all works because we have access to the raiseError function
  val success = monadErrorEither.pure(32) // This will be Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, INt] == Left("something wrong)
  // we have access to convenience methods such as

  // this is the equivalent of the recover in standard Scala (takes a function from the error type to the value type)
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  // this is the equivalent of the recoverWith which takes a function from the error type to another wrapper type.
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // This is the ErrorOr[Int]
    case _ => Left("Something else") // ErrorOr[Int]
  }

  // There is an equivalent to a "filter" type which will turn a value into an error type
  // ensure takes 3 argument lists. 1) first is wrapper type, in this case ErrorOr and we pass success
  // 2) This is the error returned in string format
  // 3) is the predicate. So if the number is less than 100 it will turn the success into a failure with the string wrapped in the left type

  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)


  // Try and Future
  import cats.instances.try_._ // this imports the implicit MonadError[Try], E = Throwable in this case
  val exception = new RuntimeException("Really  bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // when you write the raiseError this will return a Try[Nothing]

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // This would be a Future which will complete with a Failure(exception)


  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semgroup[List] => this creates an ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // have access to pure, raiseError, handleError, handlerErrorWith


  // extension methods

  import cats.syntax.applicative._ // pure is here
  import cats.syntax.applicativeError._ // this will import raiseError handlerError(With)

  val extededSuccess = 42.pure[ErrorsOr] // this requires the impliccit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int] // raiseError requires two arguments the wrapper type which is ErrorsOr and the value type here is Int
  // at the end we return the wrapper type of the value type

  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // this imports ensure
  val testedSuccess: ErrorOr[Int] = success.ensure("something bad")(_ > 100)

  def main(args: Array[String]): Unit = {
    
  }

}
