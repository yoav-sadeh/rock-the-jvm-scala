package lectures.part2afp

object PartialFunctions extends App{
  val partial125: PartialFunction[Int,Int] ={
    case 1 => 12
    case 2 => 23
    case 5 => 56
  }

  partial125(2)
  //partial125(55)

  val lifted = partial125.lift

  println(lifted(2))
  println(lifted(6))

  val chained = partial125 orElse[Int, Int] {
    case 6 => 67
  }

  println(chained(6))

  val aMappedFunction = List(1, 2, 5) map {
    case 1 => 12
    case 2 => 23
    case 5 => 56
  }

  println(aMappedFunction)



}
