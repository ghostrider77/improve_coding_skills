package greedy

import scala.annotation.tailrec

object DistinctDecomposition {
  def decomposeToDistinctElems(n: Int): List[Int] = {
    @tailrec
    def loop(number: Int, smallestSummand: Int, distinctSummands: List[Int]): List[Int] = {
      if (number == 0) distinctSummands
      else {
        val nextSummand: Int = if (number > 2 * smallestSummand) smallestSummand else number
        loop(number - nextSummand, smallestSummand + 1, nextSummand :: distinctSummands)
      }
    }
    loop(n, 1, Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: List[Int] = decomposeToDistinctElems(n)
    println(result.length)
    println(result.mkString(" "))
  }
}
