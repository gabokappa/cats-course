package part3datamanipulation

object Evaluation {

  /*
  Three forms of evaluated an expression
  1 - Eagerly
  2 - Lazily evaluating at every time you request it
  3 - Lazily but memoizing the value
   */

  import cats.Eval
  val instantEval = Eval.now { // 1-
    println("Computing now!")
    6000
  }

  val redoEval = Eval.always {
    println("Computing again!") // 2-  this doesn't automatically print to the console unlike the one above it has to be called
    4000
  }

  val delayedEval = Eval.later { // 3 - this works just like 2, but if you try and print its value to the console twice it will show the println once. It doesn't evaluated the expression again
    println("Computing later")
    5000
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  // TODO 1: (what does this print?)
  val evalEx1 = for { // now gets printed to the console event because it is eager
    a <- delayedEval // this gets evaluated so lateris printed
    b <- redoEval // this is evaluated so again is printed
    c <- instantEval // this has already been evaluated so it doesn't print again to the console
    d <- redoEval // this evaluated again so is printed to the console
  } yield a + b + c + d
  // now, later, again, again, sum

  val dontRecompute = redoEval.memoize // this will hold the internal value (so it doesn't revaluate the expression) so it remembers a computed value

  val tutorial = Eval.always {
    println("Step 1..."); "put the guitar on your lap" }
    .map { step1 => println("Step 2"); s"$step1 then put your left hand on the neck"}
    .memoize // remember the value up to this point
    .map { step12 => println("Step 3, more complicated"); s"$step12 then with the right hand play the strings"}

  // TODO 2: implement the method so that defer (Eval.now does NOT run the side effects)
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite the method with Evals instead of regular values
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head)) // adding defer here makes it stack safe. it makes it a tail recursive way

  def main(args: Array[String]): Unit = {
//    println(instantEval.value)
//    println(redoEval.value)
//    println(delayedEval.value)
//    println(delayedEval.value)
//    println(composedEvaluation.value)
//    println(composedEvaluation.value)
//    println(evalEx1.value)
//    println(evalEx1.value)
//    println(dontRecompute.value)
//    println(dontRecompute.value)
//    println(tutorial.value)
//    println(tutorial.value)
    defer(Eval.now {
      println("Now!")
      42
    })
    defer(Eval.now {
      println("Now!")
      42
    }).value // the use of .value here is that it forces the evaluation of the inner eval through the .flatMap in the defer method which then actually evualtes the expression
    println(reverseEval((1 to 10000).toList).value)
  }
}
