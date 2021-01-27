package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // this is the equivalent of the "right" value of Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("This is an error") // the left value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "left error value") // .cond tests whether the condition is true, so first argument is your condition, second is the value to return if true and the third is the string value to return if false
  // TODO: Validated is used over Either, because validated can combine a bunch of error values without mutation
  // TODO: use Either
  /*
  n must be a prime
  n must be non-negative
  n <= 100
  n must be even
   */

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d !=0 && tailRecPrime(d -1)

    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("number not even")
    val isNegative: List[String] = if (n >= 0) List() else List("number is negative")
    val isTooBig: List[String] = if (n <= 100) List() else List("number must be less than equal to 100")
    val isNotPrime: List [String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 ==0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)

  }
  // TODO: .combine in order to work because needs a Semigroup of the types used for the error in this case list of string (left) and the value (right) in this case Int. combine needs the implicit combination function so they can concatenate the lists and the other combination value of the number.
  // Here we import the semigroup for list. But because we don't want to add the numbers together we create an implicit here combineIntMax for Int, to simply return instance that returns the max value. Check .instance in here too
  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("number must be even"))
      .combine(Validated.cond(n >= 0, n ,List("number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  def main(args: Array[String]): Unit = {
    println(testNumber(150))
    println(testNumber(-100))

  }

}
