package divideandconquer

import scala.annotation.tailrec

object CountInversions {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def mergeSortedArrays(first: List[Int],
                                second: List[Int],
                                length1: Int,
                                length2: Int,
                                inversions: Long): (List[Int], Long) = {
    @tailrec
    def merge(xs: List[Int],
              xsLength: Int,
              ys: List[Int],
              mergedList: List[Int],
              accInversions: Long): (List[Int], Long) = (xs, ys) match {
      case (Nil, Nil) => (mergedList.reverse, accInversions)
      case (Nil, y :: yss) => merge(Nil, xsLength, yss, y :: mergedList, accInversions)
      case (x :: xss, Nil) => merge(xss, xsLength - 1, Nil, x :: mergedList, accInversions)
      case (x :: xss, y :: yss) =>
        if (x <= y) merge(xss, xsLength - 1, ys, x :: mergedList, accInversions)
        else merge(xs, xsLength, yss, y :: mergedList, accInversions + xsLength)
    }
    merge(first, length1, second, Nil, inversions)
  }

  def countInversions(array: List[Int], length: Int): (List[Int], Long) = {
    if (length <= 1) (array, 0L)
    else {
      val middle: Int = length / 2
      val (first, second): (List[Int], List[Int]) = array.splitAt(middle)
      val (length1, length2): (Int, Int) = (middle, length - middle)
      val (sortedFirst, inversionsInFirst): (List[Int], Long) = countInversions(first, length1)
      val (sortedSecond, inversionsInSecond): (List[Int], Long) = countInversions(second, length2)
      mergeSortedArrays(sortedFirst, sortedSecond, length1, length2, inversionsInFirst + inversionsInSecond)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val lst: List[Int] = convertToIntList(reader.next())
    val (_, result): (List[Int], Long) = countInversions(lst, n)
    println(result)
  }
}
