package divideandconquer


object MajorityElement {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def hasMajorityElem(lst: List[Int], n: Int): Boolean = {
    val counts: Map[Int, Int] = lst.groupBy(identity).mapValues(_.length)
    counts.valuesIterator.exists(_ > n / 2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val lst: List[Int] = convertToIntList(reader.next())
    val verdict: Boolean = hasMajorityElem(lst, n)
    println(if (verdict) 1 else 0)
  }
}
