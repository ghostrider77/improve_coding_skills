package minimum_spanning_trees

object CheapestRoadNetwork {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}
  import scala.collection.breakOut

  final case class Point(x: Double, y: Double)

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  private def calcDistance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)

  def calcMinimalSpanningTree(points: List[Point]): Double = {
    val nodesWithCost: MutableMap[Point, Double] = (for { p <- points } yield (p, Double.PositiveInfinity))(breakOut)
    val startingPoint: Point = points.head
    nodesWithCost(startingPoint) = 0.0

    @tailrec
    def loop(totalCost: Double): Double = {
      if (nodesWithCost.isEmpty) totalCost
      else {
        val (v, costOfAddingV): (Point, Double) = nodesWithCost.minBy { case (_, cost) => cost }
        nodesWithCost -= v
        nodesWithCost.foreach{
          case (z, costOfAddingZ) =>
            val dist: Double = calcDistance(v, z)
            if (costOfAddingZ > dist) nodesWithCost(z) = dist
        }
        loop(totalCost + costOfAddingV)
      }
    }

    loop(totalCost = 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] = (for { _ <- 0 until nrPoints } yield {
      val List(x, y): List[Double] = convertToDoubleList(reader.next())
      Point(x, y)
    }).toList
    val result: Double = calcMinimalSpanningTree(points)
    println(result)
  }
}
