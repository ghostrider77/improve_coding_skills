package dynamicprogramming

import scala.annotation.tailrec

object PrimitiveCalculator {
  private val Nominators: List[Int] = List(2, 3)

  private def findPreviousMinimum(minOperations: Array[Int], k: Int): (Int, Int) = {
    def checkForShorterCalculation(acc: (Int, Int), nominator: Int): (Int, Int) = {
      val (previousMinimum, _): (Int, Int) = acc
      if (k % nominator == 0) {
        val pos: Int = k / nominator - 1
        val nrOps: Int = minOperations(pos)
        if (nrOps < previousMinimum) (nrOps, pos)
        else acc
      }
      else acc
    }

    val position: Int = k - 2
    Nominators.foldLeft((minOperations(position), position))(checkForShorterCalculation)
  }

  private def backtrackCalculation(backtrack: Array[Int], n: Int): List[Int] = {
    @tailrec
    def loop(k: Int, path: List[Int]): List[Int] = {
      if (k <= 1) path
      else {
        val m: Int = backtrack(k)
        loop(m, (m + 1) :: path)
      }
    }
    loop(n - 1, List(n))
  }

  def runCalculator(n: Int): List[Int] = {
    val minOperations: Array[Int] = Array.fill(n)(0)
    val backtrack: Array[Int] = Array.fill(n)(0)
    for { k <- 2 to n } {
      val (previousMinimum, position): (Int, Int) = findPreviousMinimum(minOperations, k)
      minOperations(k - 1) = previousMinimum + 1
      backtrack(k - 1) = position
    }
    backtrackCalculation(backtrack, n)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: List[Int] = runCalculator(n)
    println(result.length - 1)
    println(result.mkString(" "))
  }
}
