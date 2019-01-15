package decompositions

import org.scalatest.{FreeSpec, Matchers}

class DecompositionsSuite extends FreeSpec with Matchers {

  object Helpers {
    import TopologicalOrdering.Node

    def isTopologicalSortingValid(adjacencyList: Map[Node, List[Node]], orderedNodes: List[Node]): Boolean = {
      adjacencyList.forall { case (node, neighbours) =>
        val indexOfNode: Int = orderedNodes.indexOf(node)
        neighbours.forall { orderedNodes.indexOf(_) > indexOfNode }
      }
    }
  }

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

  "DirectedGraphAcyclicity" - {
    import DirectedGraphAcyclicity.{DirectedGraph, Edge}

    "should test whether the given directed graph has a cycle" - {
      "test case 1" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(1, 2), Edge(4, 1), Edge(2, 3), Edge(3, 1))
        val graph = new DirectedGraph(edges, nrNodes)
        graph.hasCycle shouldBe true
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(1, 2), Edge(2, 3), Edge(1, 3), Edge(3, 4), Edge(1, 4), Edge(2, 5), Edge(3, 5))
        val graph = new DirectedGraph(edges, nrNodes)
        graph.hasCycle shouldBe false
      }

      "test case 3" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(1, 2), Edge(2, 3), Edge(3, 4), Edge(1, 4))
        val graph = new DirectedGraph(edges, nrNodes)
        graph.hasCycle shouldBe false
      }
    }
  }

  "TopologicalOrdering" - {
    import TopologicalOrdering.{DirectedGraph, DfsOutput, Edge, depthFirstSearch}
    import Helpers.isTopologicalSortingValid

    "should calculate a topological ordering of the nodes of a directed graph" - {
      "test case 1" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(1, 2), Edge(4, 1), Edge(3, 1))
        val graph = new DirectedGraph(edges, nrNodes)
        val DfsOutput(_, topologicalSorting) = depthFirstSearch(graph)
        isTopologicalSortingValid(graph.adjacencyList, topologicalSorting) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(4, 3, 1, 2)) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(3, 1, 4, 2)) shouldBe false
      }

      "test case 2" in {
        val nrNodes: Int = 4
        val edges: List[Edge] = List(Edge(3, 1))
        val graph = new DirectedGraph(edges, nrNodes)
        val DfsOutput(_, topologicalSorting) = depthFirstSearch(graph)
        isTopologicalSortingValid(graph.adjacencyList, topologicalSorting) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(2, 3, 1, 4)) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(2, 1, 4, 3)) shouldBe false
      }

      "test case 3" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(2, 1), Edge(3, 2), Edge(3, 1), Edge(4, 3), Edge(4, 1), Edge(5, 2), Edge(5, 3))
        val graph = new DirectedGraph(edges, nrNodes)
        val DfsOutput(_, topologicalSorting) = depthFirstSearch(graph)
        isTopologicalSortingValid(graph.adjacencyList, topologicalSorting) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(5, 4, 3, 2, 1)) shouldBe true
        isTopologicalSortingValid(graph.adjacencyList, List(1, 2, 3, 4, 5)) shouldBe false
      }
    }
  }
}
