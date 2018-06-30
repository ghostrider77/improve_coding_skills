package greedy

import scala.annotation.tailrec

object CollectingSignatures {
  final case class Segment(left: Int, right: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def getSegment(line: String): Segment = {
    val List(a, b): List[Int] = convertToIntList(line)
    Segment(a, b)
  }

  def calcMinimumNumberOfPointsCoveringSegments(segments: List[Segment]): List[Int] = {
    @tailrec
    def loop(remainingSegments: List[Segment], points: List[Int]): List[Int] = remainingSegments match {
      case Nil => points
      case Segment(_, b) :: rest => loop(rest.filter(_.left > b), b :: points)
    }
    loop(segments.sortBy(_.right), Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrSegments :Int = reader.next().toInt
    val segments: List[Segment] = (for { _ <- 0 until nrSegments } yield getSegment(reader.next())).toList
    val covering: List[Int] = calcMinimumNumberOfPointsCoveringSegments(segments)
    println(covering.length)
    println(covering.mkString(" "))
  }
}
