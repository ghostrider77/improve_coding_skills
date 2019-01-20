package minimum_spanning_trees

import org.scalatest.{FreeSpec, Matchers}

class MinimumSpanningTreesSuite extends FreeSpec with Matchers {
  private val absTol: Double = 1e-6

  "CheapestRoadNetwork" - {
    import CheapestRoadNetwork.{Point, calcMinimalSpanningTree}

    "should calculate the cheapest road network between cities" - {
      "test case 1" in {
        val points: List[Point] = List(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1))
        calcMinimalSpanningTree(points) shouldBe (3.0 +- absTol)
      }

      "test case 2" in {
        val points: List[Point] = List(Point(0, 0), Point(0, 2), Point(1, 1), Point(3, 0), Point(3, 2))
        calcMinimalSpanningTree(points) shouldBe (7.064495102 +- absTol)
      }
    }
  }

  "OptimalClustering" - {
    import OptimalClustering.{Point, calcOptimalClustering}

    "should calculate the largest distance that separates the points partitioned into k clusters" - {
      "test case 1" in {
        val nrPoints: Int = 12
        val points: List[Point] =
          List(
            Point(7, 6),
            Point(4, 3),
            Point(5, 1),
            Point(1, 7),
            Point(2, 7),
            Point(5, 7),
            Point(3, 3),
            Point(7, 8),
            Point(2, 8),
            Point(4, 4),
            Point(6, 7),
            Point(2, 6)
          )
        val k: Int = 3
        calcOptimalClustering(nrPoints, points, k) shouldBe (2.828427124746 +- absTol)
      }

      "test case 2" in {
        val nrPoints: Int = 8
        val points: List[Point] =
          List(
            Point(3, 1),
            Point(1, 2),
            Point(4, 6),
            Point(9, 8),
            Point(9, 9),
            Point(8, 9),
            Point(3, 11),
            Point(4, 12)
          )
        val k: Int = 4
        calcOptimalClustering(nrPoints, points, k) shouldBe (5.0 +- absTol)
      }
    }
  }
}
