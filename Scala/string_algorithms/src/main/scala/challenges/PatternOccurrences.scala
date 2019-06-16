package challenges

object PatternOccurrences {
  import scala.annotation.tailrec

  private val Separator: String = "$"

  private def calcPrefixFunction(string: String): List[Int] = {
    val prefixArray: Array[Int] = Array.fill(string.length)(0)

    @tailrec
    def updateBorder(border: Int, letter: Char): Int = {
      if (border > 0 && letter != string(border)) updateBorder(prefixArray(border - 1), letter)
      else border
    }

    @tailrec
    def loop(border: Int, xs: List[(Char, Int)]): Unit = xs match {
      case Nil => ()
      case (letter, ix) :: xss =>
        val updatedBorder1: Int = updateBorder(border, letter)
        val updatedBorder2: Int = if (letter == string(updatedBorder1)) updatedBorder1 + 1 else 0
        prefixArray(ix) = updatedBorder2
        loop(updatedBorder2, xss)
    }

    loop(border = 0, string.toList.zipWithIndex.drop(1))
    prefixArray.toList
  }

  def patternOccurrencesInGenome(genome: String, pattern: String): List[Int] = {
    val patternLength: Int = pattern.length
    val prefixFunction: List[Int] = calcPrefixFunction(pattern + Separator + genome)
    prefixFunction
      .zipWithIndex
      .withFilter { case (index, ix) => ix > patternLength && index == patternLength }
      .map { case (_, ix) => ix - 2 * patternLength }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val genome: String = reader.next()
    val indices: List[Int] = patternOccurrencesInGenome(genome, pattern)
    println(indices.mkString(" "))
  }
}
