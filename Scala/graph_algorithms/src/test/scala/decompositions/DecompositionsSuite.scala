package decompositions

import org.scalatest.{FreeSpec, Matchers}

class DecompositionsSuite extends FreeSpec with Matchers {

  "SameComponents" - {
    import ExitFromMaze.{Edge, Node, areNodesConnected}

    "should detect when 2 nodes are in the same components of an undirected graph" - {
      "test case 1" in {
        val edges: List[Edge] = List(Edge(1, 2), Edge(3, 2), Edge(4, 3), Edge(1, 4))
        val startNode: Node = 1
        val endNode: Node = 4
        areNodesConnected(edges, startNode, endNode) shouldBe true
      }

      "test case 2" in {
        val edges: List[Edge] = List(Edge(1, 2), Edge(3, 2))
        val startNode: Node = 1
        val endNode: Node = 4
        areNodesConnected(edges, startNode, endNode) shouldBe false
      }

      "test case 3" in {
        val edges: List[Edge] = List(Edge(1, 2), Edge(1, 3), Edge(1, 4), Edge(2, 3), Edge(3, 4), Edge(4, 5), Edge(5, 6))
        val startNode: Node = 1
        val endNode: Node = 6
        areNodesConnected(edges, startNode, endNode) shouldBe true
      }
    }
  }

  "ConnectedComponents" - {
    import ConnectedComponents.{Edge, Graph}

    "should calculate the number of connected components of an undirected graph" - {
      "test case 1" in {
        val nrNodes: Int = 4
        val edges = List(Edge(1, 2), Edge(3, 2))
        val graph: Graph = new Graph(edges, nrNodes)
        graph.connectedComponents should have length 2
      }

      "test case 2" in {
        val nrNodes: Int = 10
        val edges = List(
          Edge(1, 2),
          Edge(2, 3),
          Edge(3, 4),
          Edge(2, 4),
          Edge(3, 5),
          Edge(4, 5),
          Edge(4, 6),
          Edge(8, 7),
          Edge(9, 7)
        )
        val graph: Graph = new Graph(edges, nrNodes)
        graph.connectedComponents should have length 3
      }
    }
  }
}
