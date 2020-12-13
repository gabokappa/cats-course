package part1recap

object Implicits {

  // implicit classes (these are one argument wrapper over values)

  case class Person(name: String) {
    def greet: String = s"Hello my $name is"
  }

  implicit class ImpersonableString(name: String) { // implicit classes take a single argument all time
    def greet: String = Person(name).greet
  }

  // the below is the explicit way of calling the greet method from ImpresonableString class

//  val impersonableString = new ImpersonableString("Gabo")
//  impersonableString.greet

  val greeting = "Peter".greet // this is a new ImpersonableString("Peter").greet an extension method from the implicit class defined above

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(2) // the compiler takes the implicit argument 10
  println(incremented2)

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)


  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]") // this is a representation of an Array

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person) =
    s"""
       |{"name": "${person.name}"}
       |""".stripMargin.trim
  }
  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
  println(personsJson)
  // the implicit argument is used to PROVE THE EXISTENCE OF A TYPE

  //implicit mathods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(catName: String)
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))
// in the background it is doing val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat]) it does this automatically.
  // implicit methods are used to porive the existence of a type.


  // implicit methods can be used for conversions (but this is DISCOURAGED)


  def main(args: Array[String]): Unit = {


    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("Gabriel")))

  }
}
