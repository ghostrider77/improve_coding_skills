package paths

object ShortestPath {
  import scala.collection.mutable.{Queue => MutableQueue}

  type Node = Int

  final case class Edge(from: Node, to: Node)

  class Graph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[Node]] = Graph.createAdjacencyList(edgeList)

    def breadthFirstSearch(startNode: Node): Vector[Int] = {
      val distances: Array[Int] = Array.fill(nrNodes)(-1)
      distances(startNode - 1) = 0
      val queue: MutableQueue[Node] = MutableQueue(startNode)
      while (queue.nonEmpty) {
        val node: Node = queue.dequeue()
        val neighbours: List[Node] = adjacencyList.getOrElse(node, Nil)
        for { neighbour <- neighbours } {
          if (distances(neighbour - 1) == -1) {
            queue.enqueue(neighbour)
            distances(neighbour - 1) = distances(node - 1) + 1
          }
        }
      }
      distances.toVector
    }
  }

  object Graph {
    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] = {
      val directedEdges: List[Edge] = edgeList.flatMap{ case e @ Edge(a, b) => List(e, Edge(b, a)) }
      directedEdges.groupBy(_.from).mapValues(_.map(_.to))
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def findShortestPath(edges: List[Edge], nrNodes: Int, start: Node, end: Node): Int = {
    val graph = new Graph(edges, nrNodes)
    val distancesFromStartNode: Vector[Int] = graph.breadthFirstSearch(start)
    distancesFromStartNode(end - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b): List[Node] = convertToIntList(reader.next())
      Edge(a, b)
    }).toList
    val List(start, end): List[Node] = convertToIntList(reader.next())
    val result: Int = findShortestPath(edgeList, nrNodes, start, end)
    println(result)
  }
}
