package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

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

  // validation instances can be chained with 'andThen'. BUT its not like flatMap would short circuit if aninvalid value is passed, it would stop.

  aValidValue.andThen(_ => anInvalidValue)

  // test a valid value with ensure. Takes predicate Int => Boolean if this failes then this will turn a valid value into an invalid value
  aValidValue.ensure(List("something went wrong no divisible by 2"))(_ % 2 == 0) // if the predicate succeeds, the valid value will remain otherwise the error in the list is returned. Another form of chaining validated instances

  // using map
  aValidValue.map(_ + 1)

  // here we map the error type into something
  aValidValue.leftMap(_.length) // keeps the length of the value in the error type

  // mapping over both the error and the value
  aValidValue.bimap(_.length, _ + 1) // left habnd _.length would be the error, and the right hand _ + 1 would be the new value.

  // validated can interpolate with standard library with Either Option and Try
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here")) // returns the option which in this case is None, and if empty returns the List(Nothing present)
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("this string will throw an error".toInt)) // the Validated type changes here from String to Throwable, because this is what Try would return

  // backwards apis to turn a validated instance into an either or option
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - a form validation exercise

  import cats.instances.string._ // this is to avoid compilation errors in the validateForm function as we use .combine
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T] // this is a wrapper over a T value

    /*
    - fields: name, email and password

    rules are:
    name, email and password MUST be specified
    name must not be blank
    email must be a valid email must have @
    password must have >= 10 characters
     */

    // helper methods

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] = // we use fromOption in the validated because Map[String, String] has a get method that returns an option
      Validated.fromOption(form.get(fieldName), List(s"Field $fieldName must be specified currently empty"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must not be blank")) // if the value has more than 0 characters then it passes the value or the string error

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid")) // of the email has @ it passes the email, if not the error in the List.

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("passwords must be at least 10 characters"))


    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name").andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm))
          .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "Success user registration complete") // In theory the value here would be all concatinated because of the implicit string import earlier in the code.
  }

  // Validated type has some extension methods
  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int] // invalid[Int] because the valid option would be an Int



  def main(args: Array[String]): Unit = {
    println(testNumber(150))
    println(testNumber(-100))
    println(aValidValue.leftMap(_.length))
    println(anInvalidValue.leftMap(_ => "leftMapped produced this new error message")) // can also do the above but it retrurns the cryptic 16 which is the number of characters of the error message?
    println(aValidValue.bimap(_.length, _ + 10))
    println(optionToValidated) // change the None to a Some(Int) like Some(42) and this would return a Valid back
    println(aValidValue.toEither)
    val formToUse = Map(
      "Name" -> "Gabriel",
      "Email" -> "something@something.com",
      "Password" -> "NotARealPassword"
    )
    println(FormValidation.validateForm(formToUse))


  }

}
