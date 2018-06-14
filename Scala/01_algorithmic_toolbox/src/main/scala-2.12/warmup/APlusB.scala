package warmup

object APlusB {

  def addTwoNumbers(a: Int, b: Int): Int = a + b

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val a: Int = reader.next().toInt
    val b: Int = reader.next().toInt
    val result: Long = addTwoNumbers(a, b)
    println(result)
  }
}
