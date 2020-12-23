package part1recap

object TCVariance {

  import cats.Eq._ // here we import the type class
  import cats.instances.int._ // here and below we import type class instances // This brings into scope Eq[Int] type class instance
  import cats.instances.option._ // this allows the compiler to construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._ // here we import the extension methods for the instances

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparsion = Some(2) === None // Some(2) is an instance of Some, so it needs Eq[Some[Int]] even if Some is a sub-type of option, so we need to introduct variance

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T] // the plus sign allows the propagation of the subtype to the generic type
  val cage: Cage[Animal] = new Cage[Cat] // Cat extends Animal Cats <: ANimal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type (so the opposite of above)

  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat<: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: If a generic type HAS a T (contains a T) then it should be a covariant, And if it ACTS on T or operates on T then that should be contravariant
  // variance affect how type classes instanced are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] // this ok because the compiler can find the implicit Type Class instance defined above
  makeSound[Cat] // this is also ok because the type class instance for Animal is also applicable to Cats because SoundMaker is contravariant

  // rule 1: contravariant TCs can use superclass instances if nothing is avialbable strictly for that type.


  // this has implication for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // example with a COVARIANT

  // The below is the type class defenition
  trait AnimalShow[+T] {
    def show: String
  }

  // some type class instances that support Animal and Cat
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }

  // API for users to call
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 3: You can't have both covariant and covariant
  // CATS use INVARIANT type classes
  // use the general type with smarth constructures
  Option(2) === Option.empty[Int]


  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
    //rule 2: covariant type classes will always use the more specific type classes instance for that type. But the compiler will be confused if a general type is iinpliccit
    // println(organizeShow[Animal])
  }

}
