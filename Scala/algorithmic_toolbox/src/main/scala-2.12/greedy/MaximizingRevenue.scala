package greedy

object MaximizingRevenue {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcMaximalRevenue(profitPerClick: List[Int], averageClickPerDay: List[Int]): Long =
    profitPerClick.sorted.zip(averageClickPerDay.sorted).foldLeft(0L){ case (acc, (p, a)) => acc + p * a.toLong }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _ :Int = reader.next().toInt
    val profitPerClick: List[Int] = convertToIntList(reader.next())
    val averageClickPerDay: List[Int] = convertToIntList(reader.next())
    val result: Long = calcMaximalRevenue(profitPerClick, averageClickPerDay)
    println(result)
  }
}
