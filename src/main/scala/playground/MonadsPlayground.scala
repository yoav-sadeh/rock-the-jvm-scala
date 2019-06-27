package playground

object MonadsPlayground extends App{

  val something = Some(3)
  print(something.flatMap(x => None))

  List(List(1), List(2), List(3)).flatten

}
