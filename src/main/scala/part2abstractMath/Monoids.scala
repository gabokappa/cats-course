package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // this is so we can import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative so the result of sumLeft and sumRight should be the same

  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
// The below doesn't work because there is no value we can give as the seed/default value in the foldLeft that is T. That is what we use a Monoid for.
//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.foldLeft()(_ |+| _)

  // MONOIDS
  // Monoids similar to semigroup but have the ability to give a zero/neutral value. Monoids are basically an extension of semigroups with an empty neutral value.
  import cats.Monoid

  val intMonoid = Monoid[Int] // the compiler will fetch the implicit monoid Int
  val combineInt = intMonoid.combine(23, 999) // the monoid can combine values of the same type in this case Int
  val zero = intMonoid.empty // this will return the zero value for Int is 0.

  import cats.instances.string._ // this will bring the implicit Monoid[String] in scope so we can have an empty string
  val emptyString = Monoid[String].empty // this will return the natural element of type string is ""
  val combinedString = Monoid[String].combine("I understand ", "monoids....kinda")

import cats.instances.option._ // a wrapper type over value types. // the compiler constructs an implicit Monoid[Option[Int]] or any type
  val emptyOption = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Option(5), Option(4)) // Some(9)


  // extension methods for Monoids! - they have the same combine methods |+| this |+| is already imported above cats.syntax.semigrpup._ can also import as bellow
  //import cats.syntax.monoid._ This was commented it out to avoid confusing the compiler
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold

   def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
      list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid yourself use an import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Gabriel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._
  val mapMonoid = Monoid[Map[String, Int]]
  val massivePhoneBook = combineFold(phonebooks)

  // TODO 3: shopping cart and online stores with Monoids(so create your monoid)
  // hint: define your monoid - Monoid.instance
  // hint user combineByFold

  case class ShoppingCart(items: List[String], total: Double) // DONT USE DOUBLE FOR REAL CURRENCY
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    emptyValue = ShoppingCart(List(""), 0.0),
    cmb = (sa, sb) => ShoppingCart(sa.items ++ sb.items, sa.total + sb.total)
  )


  def main(args: Array[String]): Unit = {

    println(sumLeft)
    println(sumRight)
    println(combineOption)
    println(combineFold(List("bread ", "butter ", "pudding")))
    println(combineFold(numbers))
    println(combineFold(phonebooks))
    println(massivePhoneBook)
    println(checkout(List(
      ShoppingCart(List("iphone", "shoes"), 1000),
      ShoppingCart(List("TV", "shoes"), 800),
      ShoppingCart(List(), 0)
    )))

  }
}
