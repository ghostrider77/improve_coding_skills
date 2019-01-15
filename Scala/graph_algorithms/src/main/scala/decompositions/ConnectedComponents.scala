package decompositions

object ConnectedComponents {
  import scala.annotation.tailrec

  type Node = Int
  type Component = List[Node]

  final case class Edge(from: Node, to: Node)

  class Graph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[Node]] = Graph.createAdjacencyList(edgeList)

    lazy val connectedComponents: List[Component] = calcConnectedComponents()

    private def calcConnectedComponents(): List[Component] = {
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

      findComponents((1 to nrNodes).toList, Nil)
    }
  }

  object Graph {
    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] = {
      val directedEdges: List[Edge] = edgeList.flatMap{ case e @ Edge(a, b) => List(e, Edge(b, a)) }
      directedEdges.groupBy(_.from).mapValues(_.map(_.to))
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b): List[Node] = convertToIntList(reader.next())
      Edge(a, b)
    }).toList
    val graph: Graph = new Graph(edgeList, nrNodes)
    val components: List[Component] = graph.connectedComponents
    println(components.length)
  }
}
