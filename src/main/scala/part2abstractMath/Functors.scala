package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1,2,3).map(_+1)
  val aModifiedOption = Option(3).map(_ + 1) // Some(4)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43

  // FUNCTORS -- are a type class that will generalise the idea of a map function

  // simplified definition
  trait MyFunctor[F[_]] { // higher kinded type class as F is generic
    def map[A, B](initialValue: F[A])(f: A => B): F[B] // in this example F is the List Option or Try used above
  }

  // Cats Functor
  import cats.Functor // this is part one the type class definition
  import cats.instances.list._ // this includes Functor[List] // this is type class instance
  val listFunctor = Functor[List] // If you wanted to use a higher kinded type you would pass the generic type Functor[GenericType]. This fetches the impliccit instance
  val incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1) // List(2,3,4) // fundamental method: map

  import cats.instances.option._ // which includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._ // which includes Functor[Try]
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalise
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object whch extends Functor[Tree]
  trait Tree[+T]

  // creating a companion object with smart constructors. USE THESE SMART CONSTRUCTORS INSTEAD OF CASE CLASS
  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }


  // extension method for Functor - map (you can bring this in scope with the below, which allows you to call map on your data structure as long as you have a functor in scope
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)


  // TODO 2: write a shorter do10x method using extension methods
  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10) // in the scope is an implicit Functor of F constructed above and we've imported th extension method map that allows for the new syntax
// the [F[_] : Functor] This means there is an implicit of Functor of F in scope. This a contextbound

  def main(args: Array[String]): Unit = {

    println(do10x(List(1,2,3))) // this works because we have imported import cats.instances.list._
    println(do10x(Option(2))) // this works because we have imported import cats.instances.option._
    println(do10x(Try(35)))
    println(do10x[Tree](Branch(30, Leaf(10), Leaf(20)))) // the type class [Tree] needs to br specific so the compiler can get the value of Tree of Int not Branch Int.
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
    println(do10xShorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

  }

}
