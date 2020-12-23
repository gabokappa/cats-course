package part1recap

object TypeClasses {

  case class Person(name: String, age: Int)

  // part1 - type class definition (usually a generic trait that will be later extended)

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part2 - create implicit type class INSTANCES
implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

implicit object IntSerializer extends JSONSerializer[Int] {
  override def toJson(value: Int): String = value.toString
}
  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
        |{ "name" : ${value.name}, "age" : ${value.age} }
        |""".stripMargin.trim
  }

  // part 3 - offer some API to serialise things to JSON
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  // part 4 -extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {

    println(convertListToJSON(List(Person("Gabriel", 34), Person("Sophie", 33))))
    println(convertListToJSON(List(1, 2, 3)))

    val gabo = Person("Gabriel", 34)
    println(convertListToJSON(List(gabo)))
    import JSONSyntax._
    println(gabo.toJson)

  }
}
