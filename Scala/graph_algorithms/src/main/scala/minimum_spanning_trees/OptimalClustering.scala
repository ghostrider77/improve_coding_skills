package minimum_spanning_trees

object OptimalClustering {
  import scala.annotation.tailrec

  final case class Point(x: Double, y: Double)

  final case class PointPairDistance(indexOfP: Int, indexOfQ: Int, distance: Double)

  class UnionFind(nrPoints: Int) {
    private val parentIndices: Array[Int] = (0 until nrPoints).toArray
    private val ranks: Array[Int] = Array.fill(nrPoints)(0)

    private def changeParentsToRoot(indicesOnPath: List[Int], root: Int): Unit =
      indicesOnPath.foreach(ix => parentIndices(ix) = root)

    def find(childIndex: Int): Int = {
      @tailrec
      def loop(id: Int, parentId: Int, indicesTowardsRoot: List[Int]): (Int, List[Int]) = {
        if (id == parentId) (id, indicesTowardsRoot)
        else loop(parentId, parentIndices(parentId), id :: indicesTowardsRoot)
      }
      val (root, indicesOnPath): (Int, List[Int]) = loop(childIndex, parentIndices(childIndex), Nil)
      changeParentsToRoot(indicesOnPath, root)
      root
    }

    def union(parentIndexP: Int, parentIndexQ: Int): Unit = {
      if (parentIndexP != parentIndexQ) {
        if (ranks(parentIndexP) > ranks(parentIndexQ)) parentIndices(parentIndexQ) = parentIndexP
        else {
          parentIndices(parentIndexP) = parentIndexQ
          if (ranks(parentIndexP) == ranks(parentIndexQ)) ranks(parentIndexQ) += 1
        }
      }
    }
  }

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  private def calcDistance(p: Point, q: Point): Double = math.hypot(p.x - q.x, p.y - q.y)

  private def calcPairwisedistances(points: List[Point]): List[PointPairDistance] = {
    val distances: List[PointPairDistance] = (for {
      (p, ix) <- points.view.zipWithIndex
      (q, jy) <- points.drop(ix + 1).view.zipWithIndex
    } yield PointPairDistance(ix, ix + jy + 1, calcDistance(p, q))).toList
    distances.sortBy { case PointPairDistance(_, _, dist) => dist }
  }

  def calcOptimalClustering(nrPoints: Int, points: List[Point], k: Int): Double = {
    val distances: List[PointPairDistance] = calcPairwisedistances(points)
    val clusters = new UnionFind(nrPoints)

    @tailrec
    def loop(nrClusters: Int, xs: List[PointPairDistance]): Double = {
      val PointPairDistance(ixP, ixQ, dist) = xs.head
      val clusterOfP: Int = clusters.find(ixP)
      val clusterOfQ: Int = clusters.find(ixQ)
      if (clusterOfP == clusterOfQ) loop(nrClusters, xs.tail)
      else {
        clusters.union(clusterOfP, clusterOfQ)
        if (nrClusters == k) dist
        else loop(nrClusters - 1, xs.tail)
      }
    }

    loop(nrPoints, distances)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPoints: Int = reader.next().toInt
    val points: List[Point] = (for { _ <- 0 until nrPoints } yield {
      val List(x, y): List[Double] = convertToDoubleList(reader.next())
      Point(x, y)
    }).toList
    val k: Int = reader.next().toInt
    val d: Double = calcOptimalClustering(nrPoints, points, k)
    println(d)
  }
}
