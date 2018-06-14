package warmup

object MaximumPairwiseProduct {
  def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcMaximumPairwiseProduct(list: List[Int]): Long = {
    val (largestElem, maxIndex): (Int, Int) = list.toIterator.zipWithIndex.maxBy { case (elem, _) => elem }
    val secondLargestElem: Int =
      list.toIterator.zipWithIndex.filter { case (_, ix) => ix != maxIndex }.maxBy { case (elem, _) => elem }._1
    largestElem.toLong * secondLargestElem.toLong
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val list: List[Int] = convertToIntList(reader.next())
    val result: Long = calcMaximumPairwiseProduct(list)
    println(result)
  }

}
