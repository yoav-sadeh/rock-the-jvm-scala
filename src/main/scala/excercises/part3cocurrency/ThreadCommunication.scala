package excercises.part3cocurrency

object ThreadCommunication extends App {

  // 1. Can you think of an example where notifyAll acts differently than notify? yes - when the synchronized object is
  //  not used as data source



  //2. create a deadlock
  def deadlock(): Unit = {
    class Person(val id: Int) {
      def takeABow(person: Person): Unit = {
        this.synchronized {
          println(s"[person$id] bowing to [person${person.id}]")
          person.rise()
        }
      }

      def rise(): Unit = {
        this.synchronized {
          println(s"[person$id] rising")
        }
      }
    }

    val person1 = new Person(1)
    val person2 = new Person(2)

    new Thread(() => person1.takeABow(person2)).start()
    new Thread(() => person2.takeABow(person1)).start()
  }

  //deadlock()

  //3. create a livelock(threads are active but can not cotinue - they keep delegating to each other)
  def livelock(): Unit = {

    object Sidewalk
    class LivePerson(val id: Int, var crossed: Boolean = false) {
      def bumpInto(other: LivePerson): Unit = {
        println(s"[person$id] bumped into [person${other.id}]")
        other.synchronized {
          while(!other.crossed){
            Thread.sleep(10)
            giveWay(other)
            //other.wait()
          }
          crossed = true
          println(s"[person$id] crossed")
        }
      }


      def giveWay(other: LivePerson): Unit = {
        println(s"[person$id] trying to give way to [person${other.id}]")
        other.synchronized {
          other.notify()
          println(s"[person$id] giving way to [person${other.id}]")
        }
      }
    }
    val person1 = new LivePerson(1)
    val person2 = new LivePerson(2)
    new Thread(() => person1.bumpInto(person2)).start()
    new Thread(() => person2.bumpInto(person1)).start()
  }

  livelock()
}
