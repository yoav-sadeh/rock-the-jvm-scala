package lectures.part2afp

object CurrysAndPAF extends App{

  val superAdder: Int => Int => Int = x => y => x + y

  val add3 = superAdder(3)
  println(add3(5))

  def curriedAdder(x: Int)(y: Int): Int = x + y

  //lifted by compiler
  val add4: Int => Int = curriedAdder(4)
  println(add4(5))

  def concatenator(a: String, b: String, c: String):String = a + b + c
  val insertName = concatenator("Hello, I'm ", _, "! How are you?")
  println(insertName("Yoav"))
}
