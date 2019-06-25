package lectures.part2afp

object LazyEvaluation extends App {

  //This explodes
  //val x = throw new RuntimeException("BOOM!!!!")

  //This doesn't
  lazy val doNotUseMe = throw new RuntimeException("BOOM!!!!")

  lazy val useMeAndYouWillExplode = throw new RuntimeException("BOOM!!!!")
  try{
    println(useMeAndYouWillExplode)
  }catch{
    case e: RuntimeException => println(s"${e.getMessage}...see?")
  }

  lazy val i: Int = {
    println("evaluating...")
    42
  }

  println(i)
  println(s"let's print i again")
  println(i)

  val boolie = false

  lazy val sideEffectBoolean: Boolean = {
    println("We really need this to happen!!!")
    true
  }
  println(if(boolie && sideEffectBoolean)"OK" else "no side effects in lazy vals or by names!!")

  //Call by need
  def byNameComputationWithoutLazy(n: => Int): Int = {
    n + n
  }

  def byNameComputationWithLazy(n: => Int): Int = {
    lazy val evaluated = n
    evaluated + evaluated
  }

  def sideEffectIntFunction: Int = {
    println("waiting")
    Thread.sleep(1000)
    4
  }

  println(byNameComputationWithLazy(sideEffectIntFunction))
  println(byNameComputationWithoutLazy(sideEffectIntFunction))

  def greaterThan20(n: Int): Boolean = {
    println(s"is $n greater than 20?")
    n > 20
  }

  def lessThan30(n: Int): Boolean = {
    println(s"is $n lesser than 30?")
    n < 30
  }

  val iterator = List(1, 3, 15, 23, 26, 30, 37).withFilter(greaterThan20).withFilter(lessThan30)
  iterator.foreach(println)
}
