package paths

object MinimumCostPath {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Set => MutableSet}
  import scala.collection.breakOut

  type Node = Int
  type Component = List[Node]

  final case class Edge(from: Node, to: Node, weight: Int)
  final case class EdgeEndpoint(tip: Node, weight: Int)

  class DirectedGraph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[EdgeEndpoint]] = createAdjacencyList(edgeList)

    def dijkstraAlgorithm(start: Node): Vector[Double] = {
      val distances: Array[Double] = Array.fill(nrNodes)(Double.PositiveInfinity)
      distances(start - 1) = 0.0
      val unvisitedNodes: MutableSet[Node] = (for {
        node <- 1 to nrNodes
        if node != start
      } yield node)(breakOut)

      def updateDistances(node: Node, shortestDistanceToNode: Double): Unit = {
        val neighbours: List[EdgeEndpoint] = adjacencyList.getOrElse(node, Nil)
        for { EdgeEndpoint(v, weight) <- neighbours } {
          val distanceThroughU: Double = shortestDistanceToNode + weight
          if (distances(v - 1) > distanceThroughU) {
            distances(v - 1) = distanceThroughU
          }
        }
      }

      @tailrec
      def loop(node: Node, shortestDistanceToNode: Double): Unit = {
        updateDistances(node, shortestDistanceToNode)
        if (unvisitedNodes.nonEmpty) {
        val (u, distU): (Node, Double) =
            unvisitedNodes.map(node => (node, distances(node - 1))).minBy { case (_, dist) => dist }
          unvisitedNodes.remove(u)
          loop(u, distU)
        }
      }

      loop(start, distances(start - 1))
      distances.toVector
    }

    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[EdgeEndpoint]] =
      edgeList.groupBy(_.from).mapValues(_.map{ case Edge(_, to, weight) => EdgeEndpoint(to, weight) })
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def findCheapestPath(edgeList: List[Edge], nrNodes: Int, startNode: Node, endNode: Node): Int = {
    val graph = new DirectedGraph(edgeList, nrNodes)
    val distancesFromStart: Vector[Double] = graph.dijkstraAlgorithm(startNode)
    val result: Double = distancesFromStart(endNode - 1)
    if (result.isPosInfinity) -1 else result.toInt
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b, w): List[Node] = convertToIntList(reader.next())
      Edge(a, b, w)
    }).toList
    val List(startNode, endNode): List[Node] = convertToIntList(reader.next())
    val result: Int = findCheapestPath(edgeList, nrNodes, startNode, endNode)
    println(result)
  }
}
