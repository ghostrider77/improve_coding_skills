package introduction

object GreatestCommonDivisor {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcGCD(a: Int, b: Int): Int = if (b == 0) a else calcGCD(b, a % b)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(a, b): List[Int] = convertToIntList(reader.next())
    val result: Int = calcGCD(a, b)
    println(result)
  }
}
