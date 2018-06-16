package introduction

import scala.annotation.tailrec

object GreatestCommonDivisor {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  @tailrec
  def calcGCD(a: Int, b: Int): Int = if (b == 0) a else calcGCD(b, a % b)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(a, b): List[Int] = convertToIntList(reader.next())
    val result: Int = calcGCD(a, b)
    println(result)
  }
}
