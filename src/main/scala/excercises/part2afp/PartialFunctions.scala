package excercises.part2afp

object PartialFunctions extends App{
  val pf = new PartialFunction[Int,Int]{

    override def isDefinedAt(x: Int): Boolean = {
      x match {
        case 1 => true
        case 2 => true
        case 5 => true
        case _ => false
      }
    }

    override def apply(v1: Int): Int = v1 match {
      case 1 => 12
      case 2 => 23
      case 5 => 56

    }
  }

  println(pf(2))

  val chatbot: PartialFunction[String, String] = {
    case "Hello" => "Hello to you sir.."
    case "Goodbye" => "Goodbye to you sir.."
    case other => s"You said: $other"
  }


  scala.io.Source.stdin.getLines.map(chatbot).foreach(println)
}
