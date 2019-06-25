package excercises.part2afp

import scala.annotation.tailrec



  abstract class MyStream[+A]{
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // prepend operator
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit
    def map[B >: A](f: A => B): MyStream[B]
    def flatMap[B >: A](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]
    def filterEx(predicate: A => Boolean): MyStream[A]
    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A] = take(n).toList()

    def filterTailrec[B >: A](predicate: A => Boolean): MyStream[A]

    //Should be final to prevent overriding toList in a none @tailrec fashion
    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B]={
      if(isEmpty) acc.reverse
      else tail.toList(head :: acc)
    }
  }

  object MyStream {

    def from[A](zero: A)(generator: A => A): MyStream[A] = {
      new Cons(zero, from[A](generator(zero))(generator))
    }
  }

  object EmptyStream extends MyStream[Nothing] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new RuntimeException("AnEmptyStream doesn't have a head or tail")

    override def tail: MyStream[Nothing] = throw new RuntimeException("AnEmptyStream doesn't have a head or tail")

    override def #::[B >: Nothing](element: B): MyStream[B] = return new Cons[B](element, this)

    override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

    override def foreach(f: Nothing => Unit): Unit = ()

    override def map[B >: Nothing](f: Nothing => B): MyStream[B] = this

    override def flatMap[B >: Nothing](f: Nothing => MyStream[B]): MyStream[B] = this

    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this

    override def filterEx(predicate: Nothing => Boolean): MyStream[Nothing] = this

    //override def filterTailrec[B >: Nothing](predicate: B => Boolean): MyStream[B] = EmptyStream
    override def filterTailrec[B >: Nothing](predicate: Nothing => Boolean): MyStream[Nothing] = EmptyStream
  }

  class Cons[+A](private val hd: A, tl: => MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = false

    override val head: A = hd

    override lazy val tail: MyStream[A] = tl

    override def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)

    override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = {
      new Cons[B](head, tail ++ anotherStream)
    }

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }

    override def map[B >: A](f: A => B): MyStream[B] = {
      new Cons[B](f(head), tail.map(f))
    }

    override def flatMap[B >: A](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] = {
      if(predicate(head)){
        new Cons[A](head, tail.filter(predicate))
      }else{
        tail.filter(predicate)
      }
    }
    // In order to prevent
    override def filterEx(predicate: A => Boolean): MyStream[A] = {
        filterService(predicate, head, tail)
    }

    @tailrec
    final def filterService[B >: A](predicate: B => Boolean, hd: B, stream: => MyStream[B]): MyStream[B] ={
      if(!predicate(hd)){
        if(stream.tail == EmptyStream){
          EmptyStream
        }else {
          filterService(predicate, stream.tail.head, stream.tail)
        }
      }
      else{
        new Cons(hd, stream.filterEx(predicate))
      }

    }

    override def filterTailrec[B >: A](predicate: A => Boolean): MyStream[A] = {
      @tailrec
      def filterSpecial[B >: A](stream: MyStream[B], predicate: B => Boolean, result: MyStream[B]): MyStream[B] = {
        if (stream.isEmpty) result
        else if (predicate(stream.head)) stream.head #:: stream.tail.filter(predicate)
        else filterSpecial(stream.tail, predicate, result)
      }

      filterSpecial(this, predicate, EmptyStream)
    }

    override def take(n: Int): MyStream[A] = {
      if(n == 0) EmptyStream
      else if(n == 1) new Cons[A](head, EmptyStream)
      else new Cons[A](head, tail.take(n-1))
    }
  }

object LazyEvaluation extends App{
  val stream = MyStream.from[Int](0) (_+1)
//  println(stream.take(100000).head)
  val list = stream.take(10000).filterEx(_ < 10).toList() ///.filter(x => x < 10).takeAsList(40)
  println(list)
//  stream.take(1000).foreach(x =>{
//    print(x)
//  })
//  println()
//  println(stream.map(_ * 2).flatMap(x => new Cons(x, EmptyStream)))
//val fibonacci = MyStream.from[Tuple2[Int,Int]](0, 1){
//  case (x1:Int,x2: Int) =>  (x2, x1 + x2)
//}
//
//  //fibonacci.take(100).foreach(x => println(x._2))
//
//  def erastosthenes(numbers: MyStream[Int]):MyStream[Int] = {
//    if(numbers.isEmpty) numbers
//    else {
//      new Cons(numbers.head, erastosthenes(numbers.filter(_ % numbers.head != 0)))
//      // My initial impl
//      //      new Cons(numbers.head, primeNumbers(new Cons(numbers.head + 1, numbers))).filter(x => {
//      //        x % numbers.head != 0 || x <= numbers.head
//      //      })
//    }
//  }
//
//  val primes = erastosthenes(MyStream.from(2)(_ + 1))
//
//  primes.take(100).foreach(x => print(s"$x|"))
}
