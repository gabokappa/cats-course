package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type, it depends on the typ class instance/. Standard combination between values?

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // tha natural combitnation of two integers is an addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("good ", "bye") // concatenation

  // specific API methods

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)
  // This can also be: def reduceStrings(list: List[String]): String = list.reduce(_ + _)

  // WHAT IF we create a general API to reduce things

// The below means we can reduce strings, ints , tuples etx
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: Support a new type for a semigroup create an implicit semigroup for a new type
  // use the same pattern used in Eq in the CatsIntro
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemiGroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // EXTENSION METHODS FROM SEMIGROUP - we enhance the existing type with this combine function | + |
  import cats.syntax.semigroup._

  val anIntSum = 2 |+| 3 // will require the presence of an implicit Semigroup[Int] in scope which we importanted above. Likewise for string and int
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // TODO 2: implement reduceThings2 with the combination function

  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)

  def reduceThings3[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _) // Use a type context in the signature : Semigroup, which means the compiler will have access to Semigroup[T]
  // you basically add an impliccit paramter of Semigroup[T]

  def main(args: Array[String]): Unit = {

    println(intCombination)
    println(stringCombination)

    // specific API

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("I'm ", "doing ", "this")
    println(reduceStrings(strings))

    // general api being called
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String].
    import cats.instances.option._ // this automatically brings into scope semigrpoup of options.
    // The compiler produces an implicit semigroup[Option[Int]] - the combine will produce another option wth the summed elements. It will also produdce an implicit a semi group of optio nstrings with concatenated elements inside
    //

    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // this is an option Int containing the sum opf all the numbers. The implicit that deals iwht Option[int]] is already in scope as we imported it

    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    // test exercise one

    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 10))
    println(reduceThings(expenses))

    // ex 2

    println(reduceThings2(expenses))



  }
}
