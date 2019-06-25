package playground

import excercises.part2afp.{MyStream, Cons}

object LazyEvaluationPlayGround extends App{

  val fibonacci = MyStream.from[Tuple2[Int,Int]](0, 1){
    case (x1:Int,x2: Int) =>  (x2, x1 + x2)
  }

  //fibonacci.take(100).foreach(x => println(x._2))

  def erastosthenes(numbers: MyStream[Int]):MyStream[Int] = {
    if(numbers.isEmpty) numbers
    else {
      new Cons(numbers.head, erastosthenes(numbers.filter(_ % numbers.head != 0)))
      // My initial impl
//      new Cons(numbers.head, primeNumbers(new Cons(numbers.head + 1, numbers))).filter(x => {
//        x % numbers.head != 0 || x <= numbers.head
//      })
    }
  }



  val primes = erastosthenes(MyStream.from(2)(_ + 1))

  primes.take(100).foreach(x => print(s"$x|"))

}
