package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App{

  class SimpleContainer{
    private var value = 0

    def isEmpty: Boolean = value == 0

    def get: Int = {
      val result = value
      value = 0
      result
    }

    def set(newValue: Int): Unit = value = newValue
  }

  def prodConSimple(): Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      container.synchronized {
        println("[consumer] waiting for items...")
        container.wait()
        println(s"[conumer] consumed ${container.get}")
      }
    })

    val producer = new Thread(() => {
      container.synchronized {
        println("[producer] performing long conputation...")
        Thread.sleep(2000)
        container.set(11)
        println(s"[producer] produced 11")
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

  def prodConLargeBuffer(): Unit  = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()
      while(true){
        buffer.synchronized {
          if(buffer.isEmpty) {
            println(s"[consumer] buffer empty, waiting...")
            buffer.wait()
          }
          println(s"[consumer] popped item: ${buffer.dequeue()}")
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while(true){
        buffer.synchronized {
          if(buffer.size >= capacity) {
            println(s"[producer] buffer reached limit, waiting...")
            buffer.wait()
          }
          println(s"[producer] queueing item: $i")
          buffer.enqueue(i)
          i += 1
          buffer.notify()
        }

        Thread.sleep(random.nextInt(250))
      }})
    consumer.start()
    producer.start()
  }

prodConLargeBuffer()

}
