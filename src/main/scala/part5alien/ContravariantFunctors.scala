package part5alien

import cats.Monoid
import cats.instances.option._

object ContravariantFunctors {

  trait Format[T] { self => // self here used as an alias to this. These type classes are called contravariant
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value) // I added the type annotation

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given a specific format Format[MyType] can we have a Format[Option[MyType]] can we automatically create a format of with a wrapper (Option, FUture, List) around MyType

//  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
//    override def format(value: Option[T]): String = f.format(value.get) //get here is bad
//  }

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

//  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
//    override def format(value: A): String = f.format(func(value))
  // }


  /*
  IntFormat
  fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
  fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

  fo2 = IntFormat
    .contramap[Option[Int]](_.get)
    .contramap[Option[Option[Int]]](_.get)

    fo2.format(Option(Option(42))) =
    fo1.format(secondGet(Option(Option(42))) = // the second get is called here
    IntFormat.format(firstGet(secondGet(Option(Option(42))))

    order of operations: REVERSE from the written order
    - second get
    - first get
    - format of Int (the transformations are applied in reverse order)

    Map applies transformations in sequence (as in the sequence they were written)
    Contramap applies transformations in REVERSE sequence

   */

  // Cats has a higher-kinded contravariant type that does all the above for you
  import cats.Contravariant
  import cats.Show // this is similar to the format method above. Show has a single method that returns a value into a strong
  import cats.instances.int._ // this will import a Show of Int Show[Int] similar to the IntFormat above
  val showInts: Show[Int] = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
println(format("nothing"))
    println(format(true))
    println(format(42))
    println(format(Option(42)))
    println(format(Option(Option(42))))
  }

}
