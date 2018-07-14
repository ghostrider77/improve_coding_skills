package divideandconquer

import scala.annotation.tailrec

object OrganizingLottery {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def getNumberOfSuitableEndpoints(endpoints: Vector[Int], length: Int, point: Int): Int = {
    @tailrec
    def binarySearch(a: Int, b: Int): Int = {
      if (a == b) a
      else {
        val mid: Int = (a + b) / 2
        if (endpoints(mid) <= point) binarySearch(mid + 1, b)
        else binarySearch(a, mid)
      }
    }
    if (endpoints.last <= point) length
    else binarySearch(0, length - 1)
  }

  private def getNumberOfIntersectingSegments(sortedLeft: Vector[Int],
                                              sortedNegatedRight: Vector[Int],
                                              nrSegments: Int,
                                              point: Int): Int = {
    val nrGoodLeftEnds = getNumberOfSuitableEndpoints(sortedLeft, nrSegments, point)
    val nrGoodRightEnds = getNumberOfSuitableEndpoints(sortedNegatedRight, nrSegments, -point)
    nrGoodLeftEnds + nrGoodRightEnds - nrSegments
  }

  def numberOfSegmentsContainingPoints(leftEndpoints: Vector[Int],
                                       rightEndpoints: Vector[Int],
                                       nrSegments: Int,
                                       points: List[Int]): List[Int] = {
    val sortedLeftEndpoints: Vector[Int] = leftEndpoints.sorted
    val sortedNegatedRightEndpoints: Vector[Int] = rightEndpoints.map(elem => -elem).sorted
    points.map(getNumberOfIntersectingSegments(sortedLeftEndpoints, sortedNegatedRightEndpoints, nrSegments, _))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrSegments, _): List[Int] = convertToIntList(reader.next())
    val Vector(leftEndpoints, rightEndpoints): Vector[Vector[Int]] =
      (for { _ <- 0 until nrSegments } yield convertToIntList(reader.next())).toVector.transpose
    val points: List[Int] = convertToIntList(reader.next())
    val result: List[Int] = numberOfSegmentsContainingPoints(leftEndpoints, rightEndpoints, nrSegments, points)
    println(result.mkString(" "))
  }
}
