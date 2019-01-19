package paths

object ShortestPathInArbitraryGraph {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}

  type Node = Int

  final case class Edge(from: Node, to: Node, weight: Int)
  final case class EdgeEndpoint(tip: Node, weight: Int)

  class DirectedGraph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[EdgeEndpoint]] = createAdjacencyList(edgeList)

    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[EdgeEndpoint]] =
      edgeList.groupBy(_.from).mapValues(_.map{ case Edge(_, to, weight) => EdgeEndpoint(to, weight) })
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def updateDistances(graph: DirectedGraph,
                              distances: Array[Double],
                              collectRelaxedNodes: Boolean): List[Node] = {
    def update(acc: List[Node], nodeAndNeighbours: (Node, List[EdgeEndpoint])): List[Node] = {
      val (node, neighbours): (Node, List[EdgeEndpoint]) = nodeAndNeighbours
      val distNode: Double = distances(node - 1)
      if (distNode.isInfinite) acc
      else neighbours.foldLeft(acc){
        case (innerAcc, EdgeEndpoint(neighbour, weight)) =>
          val distanceThroughNode: Double = distNode + weight
          if (distances(neighbour - 1) > distanceThroughNode) {
            distances(neighbour - 1) = distanceThroughNode
            neighbour :: innerAcc
          } else innerAcc
      }
    }

    val updatedNodes: List[Node] = graph.adjacencyList.foldLeft(Nil: List[Node])(update)
    if (collectRelaxedNodes) updatedNodes else Nil
  }

  private def bellmanFord(graph: DirectedGraph, startNode: Node): (Array[Double], List[Node]) = {
    val n: Int = graph.nrNodes
    val distances: Array[Double] = Array.fill(n)(Double.PositiveInfinity)
    distances(startNode - 1) = 0.0

    @tailrec
    def loop(passOnEdges: Int): List[Node] = {
      if (passOnEdges < n) {
        val _: List[Int] = updateDistances(graph, distances, collectRelaxedNodes = false)
        loop(passOnEdges + 1)
      } else updateDistances(graph, distances, collectRelaxedNodes = true)
    }

    val relaxedNodes: List[Node] = loop(passOnEdges = 1)
    (distances, relaxedNodes)
  }

  private def findNodesReachableFromRelaxedNodes(relaxedNodes: List[Node], graph: DirectedGraph): Set[Node] = {
    val queue: MutableQueue[Node] = MutableQueue(relaxedNodes: _*)

    @tailrec
    def loop(visitedNodes: Set[Node]): Set[Node] = {
      if (queue.isEmpty) visitedNodes
      else {
        val node: Node = queue.dequeue()
        val neighbours: List[EdgeEndpoint] = graph.adjacencyList.getOrElse(node, Nil)
        val additionalVisitedNodes: List[Node] = neighbours.foldLeft(Nil: List[Node]){
          case (acc, EdgeEndpoint(neighbour, _)) =>
            if (!visitedNodes.contains(neighbour)) {
              queue.enqueue(neighbour)
              neighbour :: acc
            } else acc
        }
        loop(visitedNodes ++ additionalVisitedNodes)
      }
    }

    loop(relaxedNodes.toSet)
  }

  def calcShortestPaths(edges: List[Edge], nrNodes: Int, startNode: Node): List[Double] = {
    val graph = new DirectedGraph(edges, nrNodes)
    val (distances, relaxedNodes): (Array[Double], List[Node]) = bellmanFord(graph, startNode)
    val infiniteDistanceNodes: Set[Node] = findNodesReachableFromRelaxedNodes(relaxedNodes, graph)
    infiniteDistanceNodes.foreach(node => distances(node - 1) = Double.NegativeInfinity)
    distances.toList
  }

  private[paths] def convertDistanceToSymbol(dist: Double): String =
    if (!dist.isInfinity) dist.toInt.toString
    else if (dist > 0) "*"
    else "-"

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b, w): List[Node] = convertToIntList(reader.next())
      Edge(a, b, w)
    }).toList
    val startNode: Node = reader.next().toInt
    val distances: List[Double] = calcShortestPaths(edgeList, nrNodes, startNode)
    distances.foreach(convertDistanceToSymbol)
  }
}
