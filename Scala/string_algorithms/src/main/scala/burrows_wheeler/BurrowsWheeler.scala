package burrows_wheeler

object BurrowsWheeler {
  import scala.annotation.tailrec

  def calcBurrowsWheelerTransform(text: String): String = {
    val length: Int = text.length
    val doubleString: String = text * 2

    def compare(i: Int, j: Int): Boolean = {
      @tailrec
      def loop(k: Int): Boolean = {
        if (k == length) false
        else if (doubleString(i + k) < doubleString(j + k)) true
        else if (doubleString(i + k) > doubleString(j + k)) false
        else loop(k + 1)
      }
      loop(0)
    }

    val sortedIndices: List[Int] = text.indices.sortWith(compare).toList
    sortedIndices.map(ix => doubleString(ix + length - 1)).mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val result: String = calcBurrowsWheelerTransform(text)
    println(result)
  }
}
