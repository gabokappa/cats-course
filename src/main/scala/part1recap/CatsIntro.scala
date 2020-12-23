package part1recap

import cats.kernel.Eq


object CatsIntro {

  // Eq is a type class that allows you to compare values at compile time and allows the code not to compile if they are a different type

  // Eq prevents you from trying to compile the below as there is not point if the types are different
  // THIS DOESN'T COMPILE val aComparison = 2 == "a string"

  // part 1 import a class type
 // import cats.Eq._

  // part 2 import type class instances for the types you need
  import cats.instances.int._

  // part 3 - use the type class API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false as not equal
//  val anUnsafeComparison = intEquality.eqv(2, "String") // will not compile

  // part 4 - use extension methods (if applicable) cats is rich in extnesion methods
  import cats.syntax.eq._ // this is bringing into scope all the extension methods our type class supports the eq type class

  val anotherTypeSafeComparison = 2 === 3 // boolean expression thar returns false
  val neqComparison = 2 =!= 3 // true
  // val invlaidComparison = 2 === "a String" thsi doesn't compile

  // extension methods are only visible in the presence of the right type class instance

  // part 5 -extending the Type class operations to composite types e.g lists
  import cats.instances.list._ // by importing this we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3) // this returns false


  // part 6 - create a type class instance for a custom type not automatically supported by cats
  // below we create a type class instance applicable to toy cars
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lambo", 29.99) // this will be true as they have the same price

}
