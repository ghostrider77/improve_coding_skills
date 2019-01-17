package paths

object CheckBipartiteness {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue, Set => MutableSet}

  type Node = Int
  type Component = List[Node]

  final case class Edge(from: Node, to: Node)

  sealed trait NodeColor
  case object Red extends NodeColor
  case object Blue extends NodeColor
  case object Unfilled extends NodeColor

  class Graph(edgeList: List[Edge], val nrNodes: Int) {
    val adjacencyList: Map[Node, List[Node]] = createAdjacencyList(edgeList)

    lazy val isBipartite: Boolean = {
      val coloring: Array[NodeColor] = Array.fill(nrNodes)(Unfilled)

      def isVisited(node: Node) = coloring(node - 1) != Unfilled

      def getConsistentlyColoredComponent(startNode: Node): Option[MutableSet[Node]] = {
        coloring(startNode - 1) = Red
        val queue: MutableQueue[Node] = MutableQueue(startNode)
        val component: MutableSet[Node] = MutableSet(startNode)

        @tailrec
        def colorNeighbours(neighbours: List[Node], nodeColor: NodeColor): Boolean = neighbours match {
          case Nil => true
          case neighbour :: rest =>
            if (isVisited(neighbour)) {
              if (nodeColor == coloring(neighbour - 1)) false
              else colorNeighbours(rest, nodeColor)
            }
            else {
              queue.enqueue(neighbour)
              coloring(neighbour - 1) = if (nodeColor == Red) Blue else Red
              component.add(neighbour)
              colorNeighbours(rest, nodeColor)
            }
        }

        @tailrec
        def loop(): Option[MutableSet[Node]] = {
          if (queue.isEmpty) Some(component)
          else {
            val node: Node = queue.dequeue()
            val nodeColor: NodeColor = coloring(node - 1)
            val neighbours: List[Node] = adjacencyList.getOrElse(node, Nil)
            if (colorNeighbours(neighbours: List[Node], nodeColor: NodeColor)) loop()
            else None
          }
        }

        loop()
      }

      @tailrec
      def traverseComponents(unvisitedNodes: MutableSet[Node]): Boolean = unvisitedNodes.headOption match {
        case None => true
        case Some(startNode) =>
          getConsistentlyColoredComponent(startNode) match {
            case None => false
            case Some(bipartiteComponent) =>
              val remainingNodes: MutableSet[Node] = unvisitedNodes.diff(bipartiteComponent)
              traverseComponents(remainingNodes)
          }
      }
      traverseComponents(MutableSet.empty[Node] ++ (1 to nrNodes).toSet)
    }

    private def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] = {
      val directedEdges: List[Edge] = edgeList.flatMap { case e@Edge(a, b) => List(e, Edge(b, a)) }
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
    val graph = new Graph(edgeList, nrNodes)
    val result: Boolean = graph.isBipartite
    println(if (result) 1 else 0)
  }
}
