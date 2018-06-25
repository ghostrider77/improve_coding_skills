package greedy

import scala.annotation.tailrec

object ChangingMoney {
  private val OrderedCoins: List[Int] = List(10, 5, 1)

  def calcMinimumNumberOfCoins(money: Int): Int = {
    @tailrec
    def loop(amount: Int, coins: List[Int], numberOfChanges: Int): Int = coins match {
      case Nil => numberOfChanges
      case largestCoin :: remainingCoins =>
        loop(amount % largestCoin, remainingCoins, numberOfChanges + amount / largestCoin )
      }
    loop(money, OrderedCoins, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val amount: Int = reader.next().toInt
    val result: Int = calcMinimumNumberOfCoins(amount)
    println(result)
  }
}
