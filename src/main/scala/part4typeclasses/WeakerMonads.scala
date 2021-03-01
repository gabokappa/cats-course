package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] { // renamed this trait from FlatMapto MyFlatMap to not have an issue with the default FlatMap
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // hint you have access of flatmap and map

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(wf)(f => f(a)))
      //       |  |        /   \     \/
      //       |  |    M[A=>B] A=>B  B
      //       |  |    \_____  _____/
      //     M[A] A =>      M[B]
    }
  }
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // import cats.Monad  // if you look into the the Monad section you'll see it is similar to MyMonad

  import cats.FlatMap
  // FlatMap is rarely used on their own, and they have their own extension method
  import cats.syntax.flatMap._ // this provides the flatMap extension method
  import cats.syntax.functor._ // gives you a map extension method then can for comprehensions

  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  def main(args: Array[String]): Unit = {

  }

}
