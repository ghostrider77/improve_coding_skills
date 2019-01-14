package decompositions

object ExitFromMaze {
  import scala.annotation.tailrec

  type Node = Int

  final case class Edge(from: Node, to: Node)

  class Graph(edgeList: List[Edge]) {
    val adjacencyList: Map[Node, List[Node]] = Graph.createAdjacencyList(edgeList)
  }

  object Graph {
    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] = {
      val directedEdges: List[Edge] = edgeList.flatMap{ case e @ Edge(a, b) => List(e, Edge(b, a)) }
      directedEdges.groupBy(_.from).mapValues(_.map(_.to))
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def getUnvisitedNeighbours(currentNodes: Set[Node], graph: Graph, visitedNodes: Set[Node]): Set[Node] = {
    currentNodes.flatMap { node =>
      val neighbours: List[Node] = graph.adjacencyList.getOrElse(node, Nil)
      for {
        neighbour <- neighbours
        if !visitedNodes.contains(neighbour)
      } yield neighbour
    }
  }

  def areNodesConnected(edgeList: List[Edge], startNode: Node, endNode: Node): Boolean = {
    val graph: Graph = new Graph(edgeList)
    @tailrec
    def loop(currentNodes: Set[Node], visitedNodes: Set[Node]): Boolean = {
      if (currentNodes.isEmpty) false
      else {
        val unvisitedNeighbours: Set[Node] = getUnvisitedNeighbours(currentNodes, graph, visitedNodes)
        if (unvisitedNeighbours.contains(endNode)) true
        else loop(unvisitedNeighbours, visitedNodes ++ unvisitedNeighbours)
      }
    }
    loop(Set(startNode), Set(startNode))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(_, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b): List[Node] = convertToIntList(reader.next())
      Edge(a, b)
    }).toList
    val List(start, end): List[Node] = convertToIntList(reader.next())
    val result: Boolean = areNodesConnected(edgeList, start, end)
    println(if (result) 1 else 0)
  }
}
