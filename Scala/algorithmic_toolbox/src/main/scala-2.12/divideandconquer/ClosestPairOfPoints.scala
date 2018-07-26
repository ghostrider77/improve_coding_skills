package divideandconquer

object ClosestPairOfPoints {
  import Point.distance
  private val BruteForceSize: Int = 3

  final case class Point(x: Double, y: Double)

  object Point {
    def distance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)
  }

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  private def calcSmallestPairwiseDistance(points: Vector[Point],
                                           minDistance: Double,
                                           nrPointsCompareWith: Int): Double = {
    def calcDistances(currentMinDistance: Double, pointAndIndex: (Point, Int)): Double = {
      val (p, ix): (Point, Int) = pointAndIndex
      points
        .slice(ix + 1, ix + nrPointsCompareWith + 1)
        .foldLeft(currentMinDistance) { case (acc, q) =>
          val dist: Double = distance(p, q)
          if (dist < acc) dist else acc
      }
    }
    points.zipWithIndex.foldLeft(minDistance)(calcDistances)
  }

  private def findPointsInStripe(first: Vector[Point],
                                 second: Vector[Point],
                                 median: Double,
                                 delta: Double): Vector[Point] = {
    def inStripe(p: Point): Boolean = math.abs(p.x - median) <= delta
    first.filter(inStripe) ++ second.filter(inStripe)
  }

  private def calcMinimumDistanceInStripe(first: Vector[Point],
                                          second: Vector[Point],
                                          median: Double,
                                          delta: Double): Double = {
    val stripe: Vector[Point] = findPointsInStripe(first, second, median, delta)
    calcSmallestPairwiseDistance(stripe.sortBy(_.y), delta, 7)
  }

  private def findClosestPoints(sortedPoints: Vector[Point], n: Int): Double = {
    if (n <= BruteForceSize) calcSmallestPairwiseDistance(sortedPoints, Double.MaxValue, BruteForceSize - 1)
    else {
      val middleIx: Int = n / 2
      val medianX: Double = sortedPoints(middleIx).x
      val (first, second): (Vector[Point], Vector[Point]) = sortedPoints.splitAt(middleIx)
      val delta1: Double = findClosestPoints(first, middleIx)
      val delta2: Double = findClosestPoints(second, n - middleIx)
      val delta: Double = math.min(delta1, delta2)
      if (math.abs(delta) < 1e-14) 0.0
      else calcMinimumDistanceInStripe(first, second, medianX, delta)
    }
  }

  def distanceOfClosestPairOfPoints(points: Vector[Point], n: Int): Double = {
    val sortedPoints: Vector[Point] = points.sortBy(_.x)
    findClosestPoints(sortedPoints, n)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val points: Vector[Point] = (for { _ <- 0 until n } yield {
      val List(x, y): List[Double] = convertToDoubleList(reader.next())
      Point(x, y)
    }).toVector
    val result: Double = distanceOfClosestPairOfPoints(points, n)
    println(result)
  }
}
