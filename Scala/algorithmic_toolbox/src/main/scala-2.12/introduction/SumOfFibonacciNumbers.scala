package introduction

import scala.annotation.tailrec
import utils.RichNumber._

object SumOfFibonacciNumbers {
  private val Modulus: Int = 10

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

  def calcLastDigitOfTheSumOfFibonacciNumbers(n: Long): Int = {
    val p: Long = calcPisanoPeriod(Modulus)
    (calcFibonacciModulo((n + 2) % p, Modulus) - 1).mod(Modulus)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Long = reader.next().toLong
    val result: Int = calcLastDigitOfTheSumOfFibonacciNumbers(n)
    println(result)
  }
}
