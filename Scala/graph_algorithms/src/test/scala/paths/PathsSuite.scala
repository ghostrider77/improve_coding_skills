package paths

import org.scalatest.{FreeSpec, Matchers}

class PathsSuite extends FreeSpec with Matchers {

  "ShortestPath" - {
    import ShortestPath.{Edge, Node, findShortestPath}

    "should calculate the length of the shortest path between two nodes in an undirected graph" - {
      "test case 1" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(1, 2), Edge(4, 1), Edge(2, 3), Edge(3, 1))
        val startNode: Node = 2
        val endNode: Node = 4
        findShortestPath(edges, nrNodes, startNode, endNode) shouldEqual 2
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(5, 2), Edge(1, 3), Edge(3, 4), Edge(1, 4))
        val startNode: Node = 3
        val endNode: Node = 5
        findShortestPath(edges, nrNodes, startNode, endNode) shouldEqual -1
      }
    }
  }

  "CheckBipartiteness" - {
    import CheckBipartiteness.{Edge, Graph}

    "should check whether a given undirected graph is bipartite" - {
      "test case 1" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(1, 2), Edge(4, 1), Edge(2, 3), Edge(3, 1))
        val graph = new Graph(edges, nrNodes)
        graph.isBipartite shouldBe false
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(5, 2), Edge(4, 2), Edge(3, 4), Edge(1, 4))
        val graph = new Graph(edges, nrNodes)
        graph.isBipartite shouldBe true
      }
    }
  }
}
