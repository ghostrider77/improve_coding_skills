package introduction

import scala.annotation.tailrec

object HugeFibonacciModulo {
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

  def calcHugeFibonacciModulo(n: Long, modulus: Int): Int = {
    val p: Long = calcPisanoPeriod(modulus)
    calcFibonacciModulo(n % p, modulus)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(n, modulus): List[Long] = convertToLongList(reader.next())
    val result: Int = calcHugeFibonacciModulo(n, modulus.toInt)
    println(result)
  }
}
