package excercises.part3cocurrency

object Intro extends App{

  // Ex #1
  def createThread(id: Int, max: Int): Thread ={
    if(id >= max) new Thread(() =>createThread(id + 1, max).start()) else
      new Thread(() => {
        val innerThread = createThread(id + 1, max)
        innerThread.start()
        innerThread.join()
        println(s"Hello from thread #${id}")
    })
  }

  def inceptionThread(id: Int = 1, max: Int = 50): Thread = new Thread(() => {
    if(id < max) {
      val thread = inceptionThread(id + 1, max)
      thread.start()
      thread.join()
    }
    println(s"Hello from thread #${id}")
  })

  val parentThread = inceptionThread()
  parentThread.start()

  // Ex #2
  var x = 0
  val threads= (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  // The biggest possible value is 100
  // The smallest possible value is 1

  // Ex #3
  var message = ""

  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "scala is awesome"
  } )

  message = "scala sucks"

  awesomeThread.start()
  Thread.sleep(2000)
  println(message)

  // a. What's the value of message? "scala is awesome"
  // is it guaranteed? no
  // Why/not? in case of contention it isn't


}
