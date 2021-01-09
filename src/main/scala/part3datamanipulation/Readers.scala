package part3datamanipulation

object Readers {

  /*
  - configuration file => initial data structure
  - a DB layer
  - an HTTP layer
  - a business logic layer
   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // selects something from the database table and returns a stauts
    def getLastOrderId(username: String): Long = 54263 // select max(orderId) from table where username = username
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // in real life this would actually start a server
  }

  // boostrap phase when we spin up the server based on a configuration file
  val config = Configuration("gabriel", "hello1", "localhost", 1234, 8, "madeup@email.com") // after created the config object now we can insert a reader to create all the other layers

  // cats a reader is a data processing in. to inform the creation of a DbConnection based on the config

  import cats.data.Reader
  // The Reader[x, y] x = is the data type to "read" the input and the y = is the output
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword)) // the apply factory method, it takes a function from a configuration to a db connection
  // this reader is the specification on how we are going to derive a DbConnection from a configuration object. we need to inject the config into the Reader.
  val dbConn = dbReader.run(config) // The configration object is passed into .run which then runs the function outlines above that creaters the DbConnection

  // Reader[I, O] the O for Output can be transformed to something else using the map function

  val gabrielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val gabrielOrderStatus: String = gabrielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
//    val usersLastOrderStatusReader = dbReader.map(_.getOrderStatus())

    // this is identical to above
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

//    usersLastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  /*
  Pattern
  1. create the initial data structure
  2. you create a reader which specifies how that data structure will be manipulated later
  3. you can then map & flatMap the reader to produce derived information
  4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO 1 - email a user
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to $address here is the $contents"
  }

  def emailUser(username: String, userEmail: String) = {
    // fetch the status of their last order
    // email them with the email service: " Your last order has the status: (status)
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"this is the content with $orderStatus")

    emailReader.run(config)
  }

// TODO 2: what programming pattenr do Readers remind you of?
  // create an initial data strcuture => you then decide how the information flows => at the very end you call run you inject the data structure
  // DEPENDENCY INJECTION PATTERN

  def main(args: Array[String]): Unit = {
println((gabrielsOrderStatusReader))
    println(gabrielOrderStatus)
    println(getLastOrderStatus("gabriel"))
    println(emailUser("gabriel", "receriver@email.com"))
  }

}
