package burrows_wheeler

object MatchCompressedString {
  import scala.collection.breakOut
  import scala.annotation.tailrec

  private def calcFirstOccurrencePositions(string: Vector[Char]): Map[Char, Int] = {
    val letterCounts: Map[Char, Int] = string.groupBy(identity).mapValues(_.length)
    letterCounts
      .keysIterator
      .toList
      .sorted
      .foldLeft((Map.empty[Char, Int], 0)) {
        case ((acc, ix), letter) => (acc + (letter -> ix), ix + letterCounts(letter))
      }._1
  }

  private def calcCountMatrix(transformedString: Vector[Char], uniqueLetters: Set[Char]): Map[Char, Vector[Int]] = {
    val countMatrix: Map[Char, Array[Int]] =
      uniqueLetters.map(_ -> Array.fill(transformedString.length + 1)(0))(breakOut)
    for {
      (letter, ix) <- transformedString.view.zipWithIndex
      char <- uniqueLetters
    } {
      val counts: Array[Int] = countMatrix(char)
      counts(ix + 1) = if (letter == char) counts(ix) + 1 else counts(ix)
    }

    for { (letter, counts) <- countMatrix } yield letter -> counts.toVector
  }

  private def letterOccursBetweenPointers(letter: Char, lastColumn: Vector[Char], top: Int, bottom: Int): Boolean =
    lastColumn.slice(top, bottom + 1).contains(letter)

  private def patternMatching(pattern: String,
                              lastColumn: Vector[Char],
                              firstOccurrences: Map[Char, Int],
                              countMatrix: Map[Char, Vector[Int]]): Int = {
    @tailrec
    def loop(reversedPattern: List[Char], top: Int, bottom: Int): Int = reversedPattern match {
      case Nil => bottom - top + 1
      case letter :: rest =>
        if (!letterOccursBetweenPointers(letter, lastColumn, top, bottom)) 0
        else {
          val letterOccurrence: Int = firstOccurrences(letter)
          val letterCounter: Vector[Int] = countMatrix(letter)
          loop(rest, letterOccurrence + letterCounter(top), letterOccurrence + letterCounter(bottom + 1) - 1)
        }
    }
    loop(pattern.reverseIterator.toList, 0, lastColumn.length - 1)
  }

  def improvedBWPatternMatching(transformedString: Vector[Char], patterns: List[String]): List[Int] = {
    val firstOccurrences: Map[Char, Int] = calcFirstOccurrencePositions(transformedString)
    val uniqueLetters: Set[Char] = firstOccurrences.keySet
    val countMatrix: Map[Char, Vector[Int]] = calcCountMatrix(transformedString, uniqueLetters)
    patterns.map(patternMatching(_, transformedString, firstOccurrences, countMatrix))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformedString: Vector[Char] = reader.next().toVector
    val _: Int = reader.next().toInt
    val patterns: List[String] = reader.next().split(" ").toList
    val nrMatches: List[Int] = improvedBWPatternMatching(transformedString, patterns)
    println(nrMatches.mkString(" "))
  }
}
