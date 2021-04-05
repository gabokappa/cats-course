package part5alien

import cats.data.Reader

object Kleislis {

  // This function returns a wrapper (Option) over a value (String)
  val func1: Int => Option[String] = x => if ( x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // func3 = func2 and Then func1 (andThen feeds the result of func2 as the input to func1)
  // Cannot compose the below with Options
  val plainFunc1: Int => String = x => if ( x % 2 == 0) s"$x is even" else "empty"
  val plainFunc2: Int => Int = x => x * 3
  val plaintFunc3: Int => String = plainFunc2 andThen plainFunc1

  // Can use Kleisli is a wrapper over a function that returns a wrapper type over a value.
  // You also need to import the flatMap function of the wrappeer type, below we're using Option
  import cats.data.Kleisli
  import cats.instances.option._ // FlatMap[Option] this is needed to Kleisli can apply flatMap to each of the data structures of what the functions return
  // Kleisli takes 3 type arguments, the higher-kinded type, the second is the input type and the third is the output type
  // So in func1k Kleisli takes a function that takes an Int, and it wraps and Option to the output String
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2k: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3k: Kleisli[Option, Int, String] = func2k andThen func1K

  // convenience apis

  // the below works if you have a functor of option in scope
  val multiply: Kleisli[Option, Int, Int] = func2k.map( _ * 2) // x => Option (something).map(_ * 2)
  val chain: Kleisli[Option, Int, String] = func2k.flatMap(x => func1K)

  // TODO
  import cats.Id // a fake wrapper type for a normal A it just returns A
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli = is the same as the Reader
  // can replace Kleisli with a Reader

  val times2: Reader[Int, Int] = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](y => y + 4)
  val composed: Kleisli[Id, Int, Int] = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  // composed is the same as the below

  val composedFor: Kleisli[Id, Int, Int] = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    // this is kind of a dependency injection
    println(composedFor(3)) // the value 3 goes into both times2 and plus4 functions in the composedFor variable these are executed in parralel

  }
}
