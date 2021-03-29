package part4typeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding {

  // TODO - implement all in terms of foldeft

  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      // list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList) // same below
      list.foldLeft(List.empty[B])((currentList, a) => f(a) :: currentList).reverse
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      // list.foldLeft(List.empty[B])((currentList, a) => currentList ++ f(a)) // THIS IS SAME AS BELOW
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))
    }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)
    }

    def filter2[A](list: List[A])(predicate: A => Boolean): List[A] = {
      list.foldLeft(List.empty[A])((currentList, a) => if (predicate(a)) a :: currentList else currentList)
    }

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable // Fldable is a higher kinded type
  import cats.instances.list._ // to import implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1,2,3), 0)(_ + _) // this will return 6
  import cats.instances.option._ // implicit Foldable[Option]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32 (first element is what you're operating on, the second value is the seed value, in this case 30, in the second paramater list you have the function to apply to the seed value with the elements of what the initial wrapper contains, in our case Option(2)

// foldRight here is stack-safe because it uses Eval regardles of the implementation. It chains Evals together
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  // the two below are convenience methods in the case of Monoid
  import cats.instances.int._ // Monoid of int importerd
  val anotherSum = Foldable[List].combineAll(List(1,2,3))

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // needs implict Monoid[String]


  // nested data structures
  import cats.instances.vector._
  val intsNested = List(Vector(1,2,3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested) // this will combine all the vector elements inside the List

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // this requires the implicit presence of Foldable[List] to do the combine operation itself. It also requires the implicit of Monoid[Int]
  val mappedConcat2 = List(1,2,3).foldMap(_.toString) // this requires Foldable[List] and Monoid[String]

  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))
    println(filter2(numbers)(_ % 2 == 0))
    println(combineAll(numbers))

  }

}
