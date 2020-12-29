package part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.future._
  val monadList = Monad[List] // fetches the implicit Monad[List] which I've imported
  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x +1))
// Te above is recap on caplicible  Option, T, Futue

  // INTRO to EITHER

  // Right is a subt-type of Either and is a case class, the same for Left. Similar to Some for Option
  val aManualEither: Either[String, Int] = Right(42) // the desirable value is the Right Int, Left is error

  // either is also a MONAD
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Error string"))

  // imagine online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet")
    else Right("London bound")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extensions methods for for-comprehension

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

// TODO: the service layer API definition of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // generalising the the response call to work on different Monads. The type of Monad needs to be in scope
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  // DO NOT CHANGE THE CODE

  /*
  Requirements -
   */

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      payload match {
        case payload if payload.length >= 20 => None
        case _ => Some(s"Request ($payload) has been accepted")
      }
    }
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap {
    conn => OptionHttpService.issueRequest(conn, "Option HTTP service")
  }

  val responseOptionFor = for {
    conn <- OptionHttpService.getConnection(config)
    resp <- OptionHttpService.issueRequest(conn, "successful call")
  } yield resp

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  object FutureHttpService extends HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] = {
      val result = for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)
      result match {
        case Some(value) => Future(value)
        case _ => Future.failed(new RuntimeException("sorry"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): Future[String] = {
      payload match {
        case payload if payload.length >= 20 => Future("payload too long")
        case _ => Future(s"Request ($payload) has been accepted")
      }
    }
  }



  // TODO implement another HttpService with LoadingOr or ErrorOr

  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) {
        Left(new RuntimeException("Connection failed: invalid config"))
      }
      else {
        Right(Connection(cfg("host"), cfg("port")))
      }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload too long"))
      else Right(s"Request ($payload) was accepted")
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  object LoadingHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      cfg match {
        case config if (!config.contains("host") || !config.contains("port")) => Left("no config details")
        case _ => Right(Connection(cfg("host"), cfg("port")))
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if (payload.length >= 20) Left("too long")
      else Right(s"Request $payload was accepted for LoadingOr type")
  }


  def main(args: Array[String]): Unit = {

    println(responseOption)
    println(errorOrResponse)
    println(getResponse(OptionHttpService, "Hello Option"))
    println(getResponse(AggressiveHttpService, "Hello ErrorOr"))
    getResponse(FutureHttpService, "Hello Future").foreach(println)
    println(getResponse(LoadingHttpService, "Hi LoadingOr")) // this appear above the FutureHttp version
  }
}
