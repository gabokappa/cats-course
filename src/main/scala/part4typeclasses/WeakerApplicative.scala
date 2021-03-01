package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicative {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {

        override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
          val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
          ap(functionWrapper)(fb)
        }

    // here make the ap method fundamental

    // TODO implement mapN
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // This is wrapper over a and b W[(A, B)] // need to map over it
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

        def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] // this is fundamental. Apply has product.

  }
  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental method

    // These two methods below are extracted into another trait called apply

//    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
//      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
//      ap(functionWrapper)(fb)
//    }
//
//    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] = ???
  }

  import cats.Apply
  import cats.instances.option._
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x +1))(Some(2)) // This returnd Some(3)

  import cats.syntax.apply._ // holds the extension methods from apply
  val tupleOfOptions = (Option(1), Option(2), Option(3)) // can create this inside out by creating a Option((1, 2, 3))
  val optionOfTuple = tupleOfOptions.tupled // this unwraps all the options and the value wraps back this is due to the product methiod. This is Some((1, 2, 3))

  val sumOption: Option[Int] = tupleOfOptions.mapN( _ + _ + _) // the end result will be Some(6)


  def main(args: Array[String]): Unit = {

  }

}
