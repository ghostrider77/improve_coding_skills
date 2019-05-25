package burrows_wheeler

object InverseBurrowsWheeler {
  import scala.annotation.tailrec
  import scala.collection.mutable.ArrayBuffer

  final case class CharacterNumber(char: Char, number: Int)

  private def createIndexedColumn(string: List[Char]): Vector[CharacterNumber] = {
    val characterNumbers: ArrayBuffer[CharacterNumber] = ArrayBuffer()

    @tailrec
    def loop(counts: Map[Char, Int], cs: List[Char]): Vector[CharacterNumber] = cs match {
      case Nil => characterNumbers.toVector
      case c :: css =>
        val count: Int = counts(c)
        characterNumbers += CharacterNumber(c, count)
        loop(counts.updated(c, count + 1), css)
    }

    loop(Map() withDefaultValue 0, string)
  }

  def inverseBurrowsWheelerTransform(transformed: List[Char]): String = {
    val lastColumn: Vector[CharacterNumber] = createIndexedColumn(transformed)
    val firstColumn: Map[CharacterNumber, Int] = createIndexedColumn(transformed.sorted).zipWithIndex.toMap

    @tailrec
    def loop(acc: List[Char], position: Int, string: List[Char]): List[Char] = string match {
      case Nil => acc
      case _ :: rest =>
        val charNumber: CharacterNumber = lastColumn(position)
        loop(charNumber.char :: acc, firstColumn(charNumber), rest)
    }

    val reconstructed: List[Char] = loop(List('$'), 0, transformed)
    reconstructed.tail.mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformed: List[Char] = reader.next().toList
    val result: String = inverseBurrowsWheelerTransform(transformed)
    println(result)
  }
}
