package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  val pool = Executors.newFixedThreadPool(10)

  pool execute (() => {
    println("Hello")
  })

  pool execute (() => {
    Thread.sleep(1000)
    println("after 1 second")
  })

  pool execute (() => {
    Thread.sleep(2000)
    println("after 2 second")
  })


  //pool.shutdown()
  try {
    pool.shutdownNow()
    println(pool.isShutdown)

    pool execute (() => {
      println("Hello")
    })
  }
  catch {
    case e: Throwable => println("pool has shut down...")
  }

  trait Account{
    var amount: Int
  }
  case class BankAccount(var amount: Int) extends Account
  case class VolatileBankAccount(@volatile var amount: Int) extends Account

  def buy(account: BankAccount, thing: String, price: Int): Unit = {
    account.amount -= price
  }

  def safeBuy(account: BankAccount, thing: String, price: Int): Unit = {
    account.synchronized {
      account.amount -= price
    }
  }

  def volatileBuy(account: VolatileBankAccount, thing: String, price: Int): Unit = {
    account.amount -= price
  }

  def runAccountExample[A <: Account](method: Function3[A, String, Int, Unit], account: A): Unit = {
    var switch = false
    for (_ <- 0 to 1000) {

      val t1 = new Thread(() => method(account, "shirt", 3000))
      val t2 = new Thread(() => method(account, "pants", 4000))
      t1.start()
      t2.start()
      t2.join()
      t1.join()

      if (account.amount != 43000 && !switch) {
        println(s"You racist!!!  amount is : ${account.amount}")
        switch = true
      }
    }
  }

  var switch = false
  var iter = 10000
  println("buy")
  for (n <- 0 to 10000) {
    val account = BankAccount(50000)
    val t1 = new Thread(() => buy(account, "shirt", 3000))
    val t2 = new Thread(() => buy(account, "pants", 4000))
    t1.start()
    t2.start()
    Thread.sleep(10)

    if (account.amount != 43000 && !switch) {
      println(s"You racist!!!  amount is : ${account.amount} n is $n")
      iter = n
      switch = true
    }
  }
  //runAccountExample(buy, BankAccount(50000))
  println("safeBuy")
  switch = false
  for (n <- 0 to iter) {
    val account = BankAccount(50000)
    val t1 = new Thread(() => safeBuy(account, "shirt", 3000))
    val t2 = new Thread(() => safeBuy(account, "pants", 4000))
    t1.start()
    t2.start()
    Thread.sleep(10)

    if (account.amount != 43000 && !switch) {
      println(s"You racist!!!  amount is : ${account.amount} n is $n")
      switch = true
      iter = n
    }
  }
  //runAccountExample(safeBuy, BankAccount(50000))
  println("volatileBuy")

  switch = false
  for (_ <- 0 to iter) {
    val account = VolatileBankAccount(50000)
    val t1 = new Thread(() => volatileBuy(account, "shirt", 3000))
    val t2 = new Thread(() => volatileBuy(account, "pants", 4000))
    t1.start()
    t2.start()
    Thread.sleep(10)

    if (account.amount != 43000 && !switch) {
      println(s"You racist!!!  amount is : ${account.amount}")
      switch = true
    }
  }
  //runAccountExample(volatileBuy, VolatileBankAccount(50000))
}


