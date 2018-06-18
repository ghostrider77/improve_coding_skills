package introduction

import scala.annotation.tailrec
import utils.RichNumber._

object PartialSumOfFibonacciNumbers {
  private val Modulus: Int = 10

  private def convertToLongList(line: String): List[Long] = line.split(" ").map(_.toLong).toList

  private def calcPisanoPeriod(modulus: Int): Long = {
    @tailrec
    def loop(a: Int, b: Int, p: Long): Long = {
      if (a == 0 && b == 1) p
      else loop(b, (a + b) % modulus, p + 1)
    }
    loop(a = 1, b = 1, p = 1L)
  }

  private def calcFibonacciModulo(n: Long, modulus: Int): Int =
    (0L until n).foldLeft((0, 1)){ case ((a, b), _) => (b, (a + b) % modulus) }._1

  def calcLastDigitOfPartialSum(m: Long, n: Long): Int = {
    val p: Long = calcPisanoPeriod(Modulus)
    val lastDigitOfPrefixSum: Int = (calcFibonacciModulo((m + 1) % p, Modulus) - 1) % Modulus
    val lastDigitOfFullSum: Int = (calcFibonacciModulo((n + 2) % p, Modulus) - 1) % Modulus
    (lastDigitOfFullSum - lastDigitOfPrefixSum).mod(Modulus)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(m, n): List[Long] = convertToLongList(reader.next())
    val result: Int = calcLastDigitOfPartialSum(m, n)
    println(result)
  }
}
