package part3datamanipulation

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1 - Define the writers at the start
  // first type the left hand side as a log, and the right-hand side is the value you want
  val aWriter: Writer[List[String], Int] = Writer(List("Started here"), 45)
  //2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map( _ + 1) // value increases, the logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something else") // this function is applied to the logs, but doesn't change the value
  val aWriterWithBoth = aWriter.bimap(_ :+ "adding a log AND modifying the vaue at the same time", _ + 1) // both value and log change
  // can influence eachother as it returns a tuple
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "something to add in a tuple mapBoth", value + 1)
  }

  // flatMap on writers
  import cats.instances.vector._ // this imports a Semiggroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // a Monoid[List[Int]] here have to import a Monoid which has th capacity to create a zero value a default empty value
  val anEmptyWriter = aWriter.reset // clears the logs and reset only works with an implicit of Monoid type

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value // thiss extracts the value
  val logs = aWriter.written // this extracts the logs
  val(l, v) = aWriter.run // this will generate a tuple with the variables l being the logds and v the vlaues

  // TODO 1: rewrite a function which "prints" things with writers

  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n -1)
      println(n)
    }
  }

  // purely functional way to dealing with logs
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n -1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this with wrtiers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n -1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n -1)
      _ <- Writer(Vector(s"Computed sum(${n -1}) = $lowerSum"), n)
    } yield lowerSum + n
  }
implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService((Executors.newFixedThreadPool(8)))
  def main(args: Array[String]): Unit = {
println(compositeWriter.run)
    countAndSay(5)
    countAndLog(5).written.foreach(println)
    // ex 2
//    naiveSum(5)
//    sumWithLogs(100).written.foreach(println)
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))

    val logs1 = sumFuture1.map(_.written) // logs from thread 1
    val logs2 = sumFuture2.map(_.written) // logs from thread 2

  }
}
