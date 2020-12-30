package part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  // Cats has a few Monad Transformers

  // combinationof Monadic values
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // How can we transform through these without having to take the Options out of the lists etc

  // Option Transformer (OptionT)
  import cats.data.OptionT // this is option transformers
  import cats.instances.list._ // fetch an implicit OptionT[Monad[List]] in this case
  import cats.instances.future._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2))) // this reads as a list of options of Int (middle, left, right)
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option('c'), Option.empty[Char]))

  // combining the list of number options and the list of char options into a Tuple like previous lessons would be clunky as would need to extract from the option.
    // This is why OptionT is handy as it has map and flatMap. Don't need to unwrap the inner option all the time. Transformer Monads have map and flatMap

  val listOfTuples: OptionT[List, (Int, Char)] = for { // this works thanks to the import of instances list
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // Either Transformer
  import cats.data.EitherT
  // apply factory method
  // if you have a list of Eithers you'll need an EitherT. EitherT here wraps a list of eithers. But the transformers aren't limited to just list, can wrap an Either, Option, Future, Try?
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("went wrong"), Right(43), Right(2)))

  // alternative: 'left' and 'right' methods in Future instead of the list above
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))// This wraps a Future of a Right == Future(Right(45))
  // This is a wrapper of a Future of Right(45)

  // change deeply nested Either
//  val transformedEither = futureOfEither.transform {
//    case Left(undesriable) => // do another Either with undesirable
//    case Right(desirable) => // do another Either with a desirable
//  }

  /*
  TODO exercise - multi-machine cluster, measure bandwith in units. We want to allocate servers to deal with traffic spike. Sum of bandwidth should > 250
  // bandwith stored as a map
   */

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // this wraps Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable")) // compare the EitherT with the one below this one is shorter, its creating an Future(Left(something))
    case Some(b) => EitherT(Future[Either[String, Int]](Right(b)))
  }

  // TODO 1 // hint call getBandwidth twice and combine the results

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      band1 <- getBandwidth(s1)
      band2 <- getBandwidth(s2)
    } yield band1 + band2 > 250 // This returns Future[Either[String, Boolean]] it either has a Future Left with the server unreachable, or  a Future Right containing the yield expression which is either true or false
  }

  // TODO 2 // hint call canWithStandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform { // this transform transforms the either inside the future from the canWIthStandSurge
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with traffic spike $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope with traffic spike not enough bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the traffice spike")
    }

  // Future[Either[String, Boolean]] This is what is in the left-handside of the arrow (Left string or Right boolean)
  // On the right hand-side of the arrow is transformed into a Future[Either[String, String]]. The case matcher shows if it contains Ri(true) it transforms to Right(String)


  def main(args: Array[String]): Unit = {
    println(listOfTuples)
    println(listOfTuples.value) // value accesses the list of options inside the OptionT
    println(generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com"))
    val resultFuture2 = generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture2.foreach(println)
  }

}
