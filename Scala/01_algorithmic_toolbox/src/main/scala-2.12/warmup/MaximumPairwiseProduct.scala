package warmup

object MaximumPairwiseProduct {
  final case class LargestElems(largest: Int, secondLargest: Int)

  def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcMaximumPairwiseProduct(list: List[Int]): Long = {
    def processNextElem(acc: LargestElems, elem: Int): LargestElems =
      if (elem > acc.largest) LargestElems(largest = elem, secondLargest = acc.largest)
      else if (elem > acc.secondLargest) acc.copy(secondLargest = elem)
      else acc

    val largestElems: LargestElems = list.foldLeft(LargestElems(Int.MinValue, Int.MinValue))(processNextElem)
    largestElems.largest.toLong * largestElems.secondLargest.toLong
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val list: List[Int] = convertToIntList(reader.next())
    val result: Long = calcMaximumPairwiseProduct(list)
    println(result)
  }
}
