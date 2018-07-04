package divideandconquer

import scala.collection.Searching._

object BinarySearch {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def findElems(lst: List[Int], length: Int, queries: List[Int]): List[Int] = {
    for { elem <- queries } yield {
      lst.search(elem) match {
        case Found(ix) => ix
        case _ => -1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val inputList: List[Int] = convertToIntList(reader.next())
    val queryList: List[Int] = convertToIntList(reader.next())
    val result: List[Int] = findElems(inputList.tail, inputList.head, queryList.tail)
    println(result.mkString(" "))
  }
}
