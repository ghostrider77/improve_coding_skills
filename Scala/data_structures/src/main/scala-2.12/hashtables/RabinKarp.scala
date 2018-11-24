package hashtables

object RabinKarp {
  import scala.annotation.tailrec
  import scala.util.Random
  import utils.RichNumber._

  private val rng = new Random(2112L)

  private[hashtables] val Prime: Long = 1000000007

  private def polynomialHashing(string: String, prime: Long, x: Long): Int =
    string.foldRight(0L){ case (char, acc) => (acc * x + char.toInt) % prime }.toInt

  private def calcPowerOfX(x: Long, prime: Long, exponent: Int): Long =
    (0 until exponent).foldLeft(1L){ case (acc, _) => (acc * x) % prime }

  private def substringHashes(text: String, textLength: Int, patternLength: Int, prime: Long, x: Long): Vector[Int] = {
    val length: Int = textLength - patternLength + 1
    val hashValues: Array[Int] = Array.fill(length)(0)
    val lastSubstring: String = text.substring(textLength - patternLength)
    hashValues(length - 1) = polynomialHashing(lastSubstring, prime, x)
    val xPower: Long = calcPowerOfX(x, prime, patternLength)
    for { ix <- length - 2 to 0 by -1 } {
      val hashed: Long = x * hashValues(ix + 1) + text(ix).toInt - xPower * text(ix + patternLength).toInt
      hashValues(ix) = hashed.mod(prime).toInt
    }
    hashValues.toVector
  }

  private def getMatchingIndices(text: String,
                                 textLength: Int,
                                 pattern: String,
                                 patternLength: Int,
                                 patternHash: Int,
                                 hashValues: Vector[Int]): List[Int] = {
    val length: Int = textLength - patternLength + 1
    @tailrec
    def loop(ix: Int, acc: List[Int]): List[Int] = {
      if (ix == length) acc.reverse
      else if (patternHash == hashValues(ix) && pattern == text.substring(ix, ix + patternLength)) {
        loop(ix + 1, ix :: acc)
      } else loop(ix + 1, acc)
    }
    loop(0, Nil)
  }

  def findPatternInText(text: String, pattern: String, prime: Long): List[Int] = {
    val x: Long = rng.nextInt(prime.toInt - 2) + 1
    val patternLength: Int = pattern.length
    val textLength: Int = text.length
    val patternHash: Int = polynomialHashing(pattern, prime, x)
    val hashValues: Vector[Int] = substringHashes(text, textLength, patternLength, prime, x)
    getMatchingIndices(text, textLength, pattern, patternLength, patternHash, hashValues)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val text: String = reader.next()
    val indices: List[Int] = findPatternInText(text, pattern, Prime)
    println(indices.mkString(" "))
  }
}
