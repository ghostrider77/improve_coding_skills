package introduction

object LastDigitOfFibonacci {
  private val Modulus: Int = 10

  def calcFibonacciLastDigit(n: Int): Int =
    (0 until n).foldLeft((0, 1)){ case ((a, b), _) => (b, (a + b) % Modulus) }._1

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: Int = calcFibonacciLastDigit(n)
    println(result)
  }
}
