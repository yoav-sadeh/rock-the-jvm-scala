package excercises.part2afp

object Monads extends App{
  

  object Lazy{
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  class Lazy[A](value: => A) extends Comparable[Lazy[A]] {

      private lazy val internalValue = value
      def use: A = {
        internalValue
      }

     def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)

     def map[B](f: (=> A) => B): Lazy[B] = flatMap(x => Lazy(f(x)))

     def flatten[B](monad: Lazy[Lazy[B]]): Lazy[B] = monad.flatMap(x => x)

    override def compareTo(o: Lazy[A]): Int = {
      if(internalValue == o.internalValue){
        0
      } else {
        1
      }
    }
  }

  //val lazyliy = Lazi(Lazi({println("Hello yoyo")}))
  //println(lazyliy)
  val laylay = Lazy{
    println("Hello yoyo")
    42
  }

  val wayway = laylay.flatMap(x => {
    Lazy{s"it's ${x.toString} now.."}
  })
  val wayway2 = laylay.flatMap(x => {
    Lazy{s"it's ${x.toString} now.."}
  })

  wayway.use
  wayway2.use

  //Monad laws:
  val f = (x:Int) => Lazy(x * 2)
  val g = (x:Int) => Lazy(x + 4)
  val leftIdentity = Lazy{42}.flatMap(x => Lazy(x * 2)).equals(f(42))
  val rightIdentity = Lazy{42}.flatMap(Lazy.apply) eq Lazy{42}
  val associativity = Lazy{42}.flatMap(x => f(x)).flatMap(x => g(x)) eq Lazy{42}.flatMap(f(_).flatMap(x => g(x)))

  if(!leftIdentity || !rightIdentity || !associativity) throw new RuntimeException("broke the law!!")
}
