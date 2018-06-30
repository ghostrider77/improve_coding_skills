package greedy

object MaximizingSalary {
  def assembleLargestNumberFromPieces(numberStrings: List[String]): String = {
    def lessThan(number1AsString: String, number2AsString: String): Boolean =
      number1AsString + number2AsString < number2AsString + number1AsString

    numberStrings.sortWith(!lessThan(_, _)).mkString("")
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val numberStrings: List[String] = reader.next().split(" ").toList
    val result: String = assembleLargestNumberFromPieces(numberStrings)
    println(result)
  }
}
