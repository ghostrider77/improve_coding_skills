package divideandconquer

import scala.collection.Searching._

object BinarySearch {
  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def findElems(vector: Vector[Int], length: Int, queries: Vector[Int]): Vector[Int] = {
    for { elem <- queries } yield {
      vector.search(elem) match {
        case Found(ix) => ix
        case _ => -1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val inputVector: Vector[Int] = convertToIntVector(reader.next())
    val queryVector: Vector[Int] = convertToIntVector(reader.next())
    val result: Vector[Int] = findElems(inputVector.tail, inputVector.head, queryVector.tail)
    println(result.mkString(" "))
  }
}
