package burrows_wheeler

object SuffixArray {
  import scala.annotation.tailrec

  def calcSuffixArray(text: String): List[Int] = {
    val length: Int = text.length

    def compare(i: Int, j: Int): Boolean = {
      val limit: Int = math.min(length - i, length - j)
      @tailrec
      def loop(k: Int): Boolean = {
        if (k == limit) false
        else if (text(i + k) < text(j + k)) true
        else if (text(i + k) > text(j + k)) false
        else loop(k + 1)
      }
      loop(0)
    }

    text.indices.sortWith(compare).toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val result: List[Int] = calcSuffixArray(text)
    println(result.mkString(" "))
  }
}
