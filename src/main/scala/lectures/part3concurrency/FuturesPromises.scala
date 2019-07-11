package lectures.part3concurrency

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

object FuturesPromises extends App{
  def getThreadIdString: String = {
    s"thread id: ${Thread.currentThread.getId}"
  }

  // 1. Meaning Of Life
//  def calcMeaningOfLife(i: Int): Int = {
//    Thread.sleep(100)
//    println(s"calcMeaningOfLife: $getThreadIdString n: $i")
//    42
//  }

//  for (n <- 0 to 10) {
//    val meaningOfLifeFuture = Future(calcMeaningOfLife(n))
//
//    meaningOfLifeFuture.onComplete {
//      case Success(meaning) => println(s"The meaning of life is: $meaning, $getThreadIdString n: $n")
//      case Failure(e) => println(s"I failed with $e")
//    }
//  }

  // 2. Social Network

  case class Profile(id: String, name: String){
    def poke(anotherProfile: Profile): Unit = println(s"$name poking ${anotherProfile.name}")
  }

  object SocialNetwork{
    private val random = new Random()
    private val names = Map(
      "fb.id-zack" -> "Zack",
      "fb.id-bill" -> "Bill",
      "fb.id-dummy" -> "Dummy"
    )

    private val friends = Map(
      "fb.id-zack" -> "fb.id-bill"
    )

    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val friendId = friends(profile.id)
      Profile(friendId, names(friendId))
    }

    def whateverHappens: Future[Profile] = SocialNetwork.fetchProfile("fb.id-dummy")
  }

  //client call - mark poke bill
  val MARK_ID = "fb.id-zack"
  val BILL_ID = "fb.id-bill"
  val markFuture = SocialNetwork.fetchProfile(MARK_ID)
//  markFuture.onComplete {
//    case Success(mark) => SocialNetwork.fetchFriend(mark).onComplete {
//      case Success(bill) => mark.poke(bill)
//      case Failure(e) => e.printStackTrace()
//    }
//    case Failure(e) => e.printStackTrace()
//  }

  //functional composition of futures:
  // map, flatMap and filter
  val nameOnTheWall = markFuture.map(_.name)
  val marksFriend = markFuture.flatMap(SocialNetwork.fetchFriend)
  val marksFriendRestricted = marksFriend.filter(_.name.startsWith("Z"))

  // for comprehention
  for {
    mark <- SocialNetwork.fetchProfile(MARK_ID)
    bill <- SocialNetwork.fetchFriend(mark)
  } mark.poke(bill)

  //recovering
  for {
    johnDoe <- SocialNetwork.fetchProfile("fb.id-nonexisting").recover{
      case e: Throwable =>  Profile("fb.id-dummy", "forever alone")
    }
    johnDoesFriend <- SocialNetwork.fetchFriend(johnDoe).recoverWith{
      case e: Throwable =>  SocialNetwork.whateverHappens
    }
  } johnDoe.poke(johnDoesFriend)

  //fallback - happy path
  for {
    bill <- SocialNetwork.fetchProfile(BILL_ID)
    billsFriend <- SocialNetwork.fetchFriend(bill).fallbackTo(SocialNetwork.whateverHappens)
  } bill.poke(billsFriend)

  // fallback - fallback failed(bad practice to allow it to fail...)
  val result = for {
    bill <- SocialNetwork.fetchProfile(BILL_ID)
    billsFriend <- SocialNetwork.fetchFriend(bill).fallbackTo(SocialNetwork.fetchProfile("fail!!!"))
  } yield bill.poke(billsFriend)

  result.recover {
    case e: Throwable => println(s"I failed: $e")
  }
  Thread.sleep(2000)
}
