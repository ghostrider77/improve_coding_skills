package decompositions

object DirectedGraphAcyclicity {
  import scala.annotation.tailrec

  type Node = Int
  type Component = List[Node]

  final case class Edge(from: Node, to: Node)
  final case class DfsOutput(components: List[Component], visitStarted: Vector[Int], visitEnded: Vector[Int])

  class DirectedGraph(edgeList: List[Edge], val nrNodes: Int) {
    private val adjacencyList: Map[Node, List[Node]] = DirectedGraph.createAdjacencyList(edgeList)

    lazy val hasCycle: Boolean = {
      val DfsOutput(_, _,  postVisitOrdering) = depthFirstSearch
      adjacencyList.exists { case (node, neighbours) =>
        val nodeVisitEndNumber: Int = postVisitOrdering(node - 1)
        neighbours.exists { neighbour => nodeVisitEndNumber <= postVisitOrdering(neighbour - 1) }
      }
    }

    private def depthFirstSearch: DfsOutput = {
      val visitStarted: Array[Int] = Array.fill(nrNodes)(0)
      val visitEnded: Array[Int] = Array.fill(nrNodes)(0)
      val previsitId: Iterator[Int] = Iterator.from(1)
      val postvisitId: Iterator[Int] = Iterator.from(1)

      def isNodeVisited(node: Node): Boolean = visitStarted(node - 1) > 0

      def findUnvisitedNeighbour(node: Node): Option[Node] =
        adjacencyList.getOrElse(node, Nil).find(!isNodeVisited(_))

      @tailrec
      def findComponents(nodes: List[Node], components: List[Component]): List[Component] = nodes match {
        case Nil => components
        case node :: remainingNodes =>
          if (isNodeVisited(node)) findComponents(remainingNodes, components)
          else {
            val currentComponent: Component = explore(node)
            findComponents(remainingNodes, currentComponent :: components)
          }
      }

      def explore(startingNode: Node): Component = {
        @tailrec
        def traverseComponent(previsitStack: List[Node], component: Component): Component = previsitStack match {
          case Nil => component
          case node :: restOfStack =>
            findUnvisitedNeighbour(node) match {
              case Some(neighbour) =>
                visitStarted(neighbour - 1) = previsitId.next()
                traverseComponent(neighbour :: previsitStack, neighbour :: component)
              case None =>
                visitEnded(node - 1) = postvisitId.next()
                traverseComponent(restOfStack, component)
            }
        }

        visitStarted(startingNode - 1) = previsitId.next()
        traverseComponent(List(startingNode), List(startingNode))
      }

      val components: List[Component] = findComponents((1 to nrNodes).toList, Nil)
      DfsOutput(components, visitStarted.toVector, visitEnded.toVector)
    }
  }

  object DirectedGraph {
    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] =
      edgeList.groupBy(_.from).mapValues(_.map(_.to))
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b): List[Node] = convertToIntList(reader.next())
      Edge(a, b)
    }).toList
    val graph: DirectedGraph = new DirectedGraph(edgeList, nrNodes)
    val verdict: Boolean = graph.hasCycle
    println(if (verdict) 1 else 0)
  }
}
