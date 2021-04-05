package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import cats.{Applicative, Foldable, Functor, Monad}

object Traversing {

  // working an example with futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // imagine pinging a server
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  // This returns a future because pinging a service and waiting for a response is an asynchronous operation
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  // need to combine the accumulator future by call the method inbetween the {} starting at the end of line 17
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)

    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  /*
  we have
  - a list of string List[String]
  - a function that turns each String is turned into a Future[Int]
  we want a Future[List[Int]] which is obtained from combining all the values

   */

  // traverse takes a container M[A] and a function that turns an elements of a Future of something else Future[B]
  // traverse here does all for comprehension that happens in allBandwidths but in a more concise and elegant way
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  // Sequence takes a container of futures and returns a future of containers. The server.map(getBandwidth) This will return a list of futures. If you do a sequence on that it turns that l;ist of futures into a Future List.
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1 This traverse operates on any kind of higher kinded type with a monad ins scope.
  // If applying the example above F becomes Future, A is the string and B is the Int

  import cats.syntax.applicative._ // importing here the pure method
  import cats.syntax.flatMap._ // import flatMap
  import cats.syntax.functor._ // this is for map

  // generalise th concept of traverse bellow. The ,pure[F] means that the Empty list of B turns into a F of empty list of B.

  def listTraverse[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement: F[B] = func(element)
      for {
        acc <- wAccumulator
        elem <-wElement
      } yield acc:+ elem
    }
  }

  //differet version of listTraverse
  import cats.syntax.apply._
  import cats.syntax.applicative._
// method implementation below is more abstract the minimum requirement is an applicative not a Monad
  def listTraverse2[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement: F[B] = func(element)
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  }

  // TODO 2
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = {
    listTraverse2(list)(identity)
  }

  // TODO 3
  import cats.instances.vector._
  val allPairs: Seq[List[Int]] = listSequence(List(Vector(1,2), Vector(3,4))) // Vectore[List[Int]] - will contain all the possible tuples ( 2- tuples)
  val threePairs: Seq[List[Int]] = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))) // what are these?

  import cats.instances.option._
  import cats.syntax.list._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse2[Option, Int, Int](list)(n => Some(n).filter(predicate))

  // TODO 4 - what is the result of these expression This is equivalent to forAll
  val allTrue = filterAsOption(List(2,4,6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse = filterAsOption(List(1,2,3))(_ % 2 == 0) // None

  import cats.data.Validated
  import cats.instances.list._ // this imports Semigroup[List] so we can generate Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse2[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // TODO 5 - what is the result of
  val allTrueValidated = filterAsValidated(List(2,4,6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated = filterAsValidated(List(1,2,3))(_ % 2 == 0) // Invalid(List("predicate for 1", "predicate for 3"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TODO 6 how to implement this method in general terms
    // type Identity[T] = T
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // this importd the implicit appicative of Future. Applicative[Future]
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods for traverse

  import cats.syntax.traverse._ // which has the sequence and traverse
  val allBandwidthsCats2 = servers.traverse(getBandwidth)


  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(threePairs)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
