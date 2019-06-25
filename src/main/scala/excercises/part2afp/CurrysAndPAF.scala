package excercises.part2afp

object CurrysAndPAF extends App {
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int): Int = x + y

  def curriedAddMethod(x: Int)(y: Int): Int = x + y

  //Create as many add7 as possible

  val add7_1 = simpleAddFunction.curried(7)
  println(add7_1(1))

  val add7_2: Int => Int = x => simpleAddFunction(7, x)
  println(add7_2(1))

  val add7_3: Int => Int = curriedAddMethod(7)
  println(add7_3(1))

  val add7_4: Int => Int = x => curriedAddMethod(7)(x)
  println(add7_4(1))

  val add7_5 = curriedAddMethod(7) _
  println(add7_5(1))

  val add7_6 = curriedAddMethod(7)(_)
  println(add7_6(1))

  val add7_7 = simpleAddMethod(7, _)
  println(add7_7(1))

  val add7_8 = simpleAddFunction(7, _)
  println(add7_8(1))

  /*
  Process a list of numbers and return their string representation with %4.2f , %8.6f and %14.12f formats as
  curried functions
  * */

  def getFormatted(list: List[Double]): List[Tuple3[String, String, String]] = {
    def formatter(format: Float)(x: Double) = s"%${format}f".format(x)

    val fourTwo = formatter(4.2f) _
    val eightSix = formatter(8.6f) _

    val fourteenTwelve = formatter(14.12f) _

    list.map(x => (fourTwo(x), eightSix(x), fourteenTwelve(x)))
  }

  println(getFormatted(List(Math.PI, 2, 3, 4, 5)))

  def byName(n: => Int): Int = n + 1

  def byFunction(f: () => Int): Int = f() + 1

  def method: Int = 42

  def paretMethod(): Int = 42

  println(byName(method))
  println(byFunction(method _))
  println(byName(paretMethod))
  println(byFunction(paretMethod))
  println(byName(43))
  println(byFunction(() => 3))

}
