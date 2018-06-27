package greedy

import scala.annotation.tailrec

object FractionalKnapsack {
  final case class Item(value: Int, weight: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readItems(lines: Iterator[String], nrItems: Int): List[Item] =
    (for { _ <- 0 until nrItems } yield {
      val List(v, w): List[Int] = convertToIntList(lines.next())
      Item(v, w)
    }).toList

  def solveFractionalKnapsack(items: List[Item], capacity: Int): Double = {
    @tailrec
    def loop(currentCapacity: Int, xs: List[Item], totalValue: Double): Double = {
      if (xs.isEmpty || currentCapacity == 0) totalValue
      else {
        val Item(value, weight): Item = xs.head
        val usedAmount: Int = weight min currentCapacity
        loop(currentCapacity - usedAmount, xs.tail, totalValue + usedAmount * (value / weight.toDouble))
      }
    }

    val sortedItems: List[Item] = items.sortBy { case Item(v, w) => v / w.toDouble }(Ordering[Double].reverse)
    loop(capacity, sortedItems, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrItems, capacity): List[Int] = convertToIntList(reader.next())
    val items: List[Item] = readItems(reader, nrItems)
    val result: Double = solveFractionalKnapsack(items, capacity)
    println(result)
  }
}
