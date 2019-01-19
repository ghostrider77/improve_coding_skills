package paths

object NegativeCycle {
  import scala.annotation.tailrec

  type Node = Int

  final case class Edge(from: Node, to: Node, weight: Int)
  final case class EdgeEndpoint(tip: Node, weight: Int)

  class DirectedGraph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[EdgeEndpoint]] = createAdjacencyList(edgeList)

    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[EdgeEndpoint]] =
      edgeList.groupBy(_.from).mapValues(_.map{ case Edge(_, to, weight) => EdgeEndpoint(to, weight) })
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def updateDistances(graph: DirectedGraph, distances: Array[Int]): Boolean = {
    def update(acc: Boolean, nodeAndNeighbours: (Node, List[EdgeEndpoint])): Boolean = {
      val (node, neighbours): (Node, List[EdgeEndpoint]) = nodeAndNeighbours
      val distNode: Int = distances(node - 1)
      val anyNeighbourUpdated: Boolean = neighbours.foldLeft(false){
        case (updateHappened, EdgeEndpoint(neighbour, weight)) =>
          val distanceThroughNode: Int = distNode + weight
          if (distances(neighbour - 1) > distanceThroughNode) {
            distances(neighbour - 1) = distanceThroughNode
            true
          } else updateHappened
      }
      acc || anyNeighbourUpdated
    }

    graph.adjacencyList.foldLeft(false)(update)
  }

  private def hasNegativeCycle(graph: DirectedGraph): Boolean = {
    val n: Int = graph.nrNodes
    val distances: Array[Int] = Array.fill(n)(0)
    @tailrec
    def loop(completePassOnEdges: Int): Boolean = {
      if (completePassOnEdges > n) false
      else {
        val isSomeEdgeUpdated: Boolean = updateDistances(graph, distances)
        if (completePassOnEdges == n && isSomeEdgeUpdated) true
        else loop(completePassOnEdges + 1)
      }
    }
    loop(completePassOnEdges = 1)
  }

  def detectNegativeCycle(edgeList: List[Edge], nrNodes: Int): Boolean = {
    val graph = new DirectedGraph(edgeList, nrNodes)
    hasNegativeCycle(graph)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b, w): List[Node] = convertToIntList(reader.next())
      Edge(a, b, w)
    }).toList
    val result: Boolean = detectNegativeCycle(edgeList, nrNodes)
    println(if (result) 1 else 0)
  }
}
