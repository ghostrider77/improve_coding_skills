package decompositions

object StronglyConnectedComponents {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  type Node = Int
  type Component = List[Node]

  final case class Edge(from: Node, to: Node)
  final case class DfsOutput(components: List[Component],
                             postVisitNumbers: Vector[Node],
                             topologicalOrdering: List[Node])

  class DirectedGraph(edgeList: List[Edge], val nrNodes: Int, nodeOrder: Option[List[Node]] = None) {
    val adjacencyList: Map[Node, List[Node]] = createAdjacencyList(edgeList)
    val orderedNodes: List[Node] = getOrderedNodes(nodeOrder)

    private def getOrderedNodes(nodeOrder: Option[List[Node]]): List[Node] = nodeOrder match {
      case None => (1 to nrNodes).toList
      case Some(nodes) => nodes
    }

    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] =
      edgeList.groupBy(_.from).mapValues(_.map(_.to))
  }

  class DepthFirstSearch(graph: DirectedGraph) {
    lazy val dfsOutput: DfsOutput = depthFirstSearch()

    private val visitStarted: Array[Int] = Array.fill(graph.nrNodes)(0)
    private val visitEnded: Array[Int] = Array.fill(graph.nrNodes)(0)
    private val topologicalSorting: ListBuffer[Node] = ListBuffer()
    private val previsitId: Iterator[Int] = Iterator.from(1)
    private val postvisitId: Iterator[Int] = Iterator.from(1)

    private def isNodeVisited(node: Node): Boolean = visitStarted(node - 1) > 0

    private def findUnvisitedNeighbour(node: Node): Option[Node] =
      graph.adjacencyList.getOrElse(node, Nil).find(!isNodeVisited(_))

    private def depthFirstSearch(): DfsOutput = {
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
                topologicalSorting += node
                traverseComponent(restOfStack, component)
            }
        }
        visitStarted(startingNode - 1) = previsitId.next()
        traverseComponent(List(startingNode), List(startingNode))
      }

      val components: List[Component] = findComponents(graph.orderedNodes, Nil)
      DfsOutput(components, visitEnded.toVector, topologicalSorting.toList.reverse)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def createGraphWithEdgesReversed(edges: List[Edge],
                                           nrNodes: Int,
                                           postVisitOrdering: Vector[Int]): DirectedGraph = {
    val reversedEdges: List[Edge] = edges.map{ case Edge(from, to) => Edge(to, from) }
    val nodeOrder: List[Node] =
      postVisitOrdering.zip(1 to nrNodes)
        .sortBy{ case (postNumber, _) => postNumber }(Ordering[Int].reverse)
        .unzip
        ._2
        .toList
    new DirectedGraph(reversedEdges, nrNodes, Some(nodeOrder))
  }

  def calcStronglyConnectedComponents(edgeList: List[Edge], nrNodes: Int): List[Component] = {
    val graph = new DirectedGraph(edgeList, nrNodes)
    val forwardDFS = new DepthFirstSearch(graph)
    val DfsOutput(_, postVisitNumbers, _) = forwardDFS.dfsOutput
    val reversedGraph: DirectedGraph = createGraphWithEdgesReversed(edgeList, nrNodes, postVisitNumbers)
    val backwardDFS = new DepthFirstSearch(reversedGraph)
    val DfsOutput(components, _, _) = backwardDFS.dfsOutput
    components
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrNodes, nrEdges): List[Int] = convertToIntList(reader.next())
    val edgeList: List[Edge] = (for { _ <- 0 until nrEdges } yield {
      val List(a, b): List[Node] = convertToIntList(reader.next())
      Edge(a, b)
    }).toList
    val scc: List[Component] = calcStronglyConnectedComponents(edgeList, nrNodes)
    println(scc.length)
  }
}
