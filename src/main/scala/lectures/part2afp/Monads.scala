package lectures.part2afp

object Monads extends App{
  trait Attempt[+A]{
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt{
    def apply[A](value: A): Attempt[A] ={
      try{
        Success(value)
      }catch {
        case e: Throwable => Fail(e)
      }
    }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
      try{
        f(value)
      }catch{
        case e: Throwable => Fail(e)
      }
    }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }


  println(Attempt(throw new RuntimeException("Hell yeah!!!!")))
}

/*
Monad laws:
1. left identity: apply/unit.flatMap(f) == f(v)
2. right identity: l.flatMap(unit/apply) == l
3. associativity:  l.flatMap(f).flatMap(g) == l.flatMap(f(_).flatMap(g))
  a. l.flatMap(f).flatMap(g) == f(v).flatMap(g)
  b. l.flatMap(f(_).flatMap(g)) == f(v).flatMap(g)
 */