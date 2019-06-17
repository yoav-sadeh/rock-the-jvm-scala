package excercises.part2afp

import scala.annotation.tailrec

object Collections extends App{
  trait MySet[A] extends (A => Boolean) {
    override def apply(v1: A): Boolean = contains(v1)

    def contains(a: A): Boolean

    def +(a: A): MySet[A]

    def ++(anotherSet: MySet[A]): MySet[A]

    def map[B](func: A => B): MySet[B]

    def flatMap[B](func: A => MySet[B]): MySet[B]

    def filter(predicate: A => Boolean): MySet[A]

    def foreach(func: A => Unit): Unit
  }

  object MySet {
    def apply[A](values: A*): MySet[A] = {
      @tailrec
      def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {

      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head) //ANonEmptySet(values.head, apply(values.tail:_*))
      }
      buildSet(values.toSeq, new EmptySet[A])
    }
  }

  class EmptySet[A] extends MySet[A] {

    override def contains(a: A): Boolean = false

    override def +(a: A): MySet[A] = ANonEmptySet(a, this)

    override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

    override def map[B](func: A => B): MySet[B] = new EmptySet[B]

    override def flatMap[B](func: A => MySet[B]): MySet[B] = new EmptySet[B]

    override def filter(predicate: A => Boolean): MySet[A] = this

    override def foreach(func: A => Unit): Unit = ()


  }
  case class ANonEmptySet[A](private var head: A, private var tail: MySet[A]) extends MySet[A] {

    override def contains(a: A): Boolean =
      a == head || (tail contains a)



    override def +(a: A): MySet[A] =
      if(contains(a)) this
      else ANonEmptySet(a, this)

    override def ++(anotherSet: MySet[A]): MySet[A] =
      tail ++ anotherSet + head

    override def map[B](func: A => B): MySet[B] = (tail map func) + func(head)
      //ANonEmptySet(func(head), tail map func)

    override def flatMap[B](func: A => MySet[B]): MySet[B] = (tail flatMap func) ++ func(head)

    override def filter(predicate: A => Boolean): MySet[A] = {
      val filteredTail = tail filter predicate
      if (predicate(head)) filteredTail + head
      else filteredTail
    }

    override def foreach(func: A => Unit): Unit = {
      func(head)
      tail foreach func
    }
  }

  val mySet = MySet(1, 2, 3, 4, 5, 6, 7)
  mySet filter(el => {
    el != 2
  }) foreach println

}

