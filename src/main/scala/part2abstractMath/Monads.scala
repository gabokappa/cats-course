package part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1,2,3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1 how do you create all combinations of (number, char)?

  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c) // a for - comprehension is being collapsed by the compiler as a chain of maps and flatmaps

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.2 how do you create the combination of (number, char)?
  // options have flatmaps and mapts

  val combinationOption = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combOptions = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures // this will be the fplatform from which the futures will be running on
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42) // a Future containign a number that will be evaluated at some point in the future in some thread
  val charFuture = Future('Z')

  // TODO 1.3 how do you create the combination of (number, char)?

  val combinationFuture = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val futuresCombined = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
  Pattern consists of two fundamental operations
  - wrapping the value into a M (monadic) value
  - the flatMap mechanism - the ability to transform M values

  MONADS - is a type class a higher-kinded CATS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // formalising the ability to turn a value into an M(value)
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // TODO implement map
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // Import the Cats Monad
  import cats.Monad
  import cats.instances.option._ // import the implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // this just creates an Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO 2: Use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future] // this requires an implicit execution context which we defined above
  val aFuture = futureMonad.pure(43) // ""
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => if (x > 6) Future(true) else Future(false)) // Future(Success(true))

  // specialised API for different types
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], chars: Option[Char]): Option[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], chars: Future[Char]): Future[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))

  // genralising with Monads
  // this can flatMap any data structure as long as an implicit Monad M in scope
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = // this requires an implicit Monad in scope, a Monad M
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))


// extension methods for Monads - weirder more complext imports - pure and flatMap are the extension methods of Monads
  import cats.syntax.applicative._ // this contains the pure extension method
  val oneOption = 1.pure[Option] // this requires an implicit Applicative[F] a Monad will be brought in scope here it uses the implicit Monad[Option] // this will be Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad. it has the same signature as the Functor map
  // see MyMonad. Monads extend Functors, Monads have access to Functor extension methods

  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)

  import cats.syntax.functor._ // this contains map
  val oneOptionMapped2 = oneOption.map(_ + 2) // map already present in the option type so the import above fades.

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  // check the signature of the original here with the impliciti signalled: def getPairsFor[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
  def getPairsFor[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b) // the above is the same as ma.flatMap(a => mb.map(b => (a, b)))


  def main(args: Array[String]): Unit = {

    println(numbersList.map(x => charsList.map(s => s"$x, $s")))
    println(combinationsList)
    println(combinationsListFor)
    println(combOptions)
    println(futuresCombined)
    println(aTransformedFuture)
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    println(getPairs(numberFuture, charFuture)) // because the result is a Future it doesn't actually compute return a result
    getPairs(numberFuture, charFuture).foreach(println)
    println(getPairsFor(numbersList, charsList))

  }

}
