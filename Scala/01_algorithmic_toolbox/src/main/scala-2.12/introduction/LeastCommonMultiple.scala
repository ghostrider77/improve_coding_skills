package introduction

object LeastCommonMultiple {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def calcGCD(a: Int, b: Int): Int = if (b == 0) a else calcGCD(b, a % b)

  def calcLCM(a: Int, b: Int): Long = {
    val gcd: Int = calcGCD(a, b)
    (a / gcd) * b.toLong
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(a, b): List[Int] = convertToIntList(reader.next())
    val result: Long = calcLCM(a, b)
    println(result)
  }
}
