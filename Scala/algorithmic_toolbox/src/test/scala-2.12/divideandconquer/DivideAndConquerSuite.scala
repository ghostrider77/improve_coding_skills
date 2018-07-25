package divideandconquer

import org.scalatest.{FreeSpec, Matchers}

class DivideAndConquerSuite extends FreeSpec with Matchers {

  "BinarySearch" - {
    import BinarySearch.findElems

    "should find the index of a given number in a sorted vector" in {
      val vec: Vector[Int] = Vector(2, 5, 6, 7, 10, 12, 20)
      val n: Int = vec.length
      val queries: Vector[Int] = Vector(1, 5, 8, 10, 15, 20, 25)
      findElems(vec, n, queries) shouldEqual Vector(-1, 1, -1, 4, -1, 6, -1)
    }
  }

  "MajorityElement" - {
    import MajorityElement.hasMajorityElem

    "should return true if there is a majority elem in the list, otherwise it should be false" in {
      hasMajorityElem(List(2, 3, 9, 2, 2), 5) shouldBe true
      hasMajorityElem(List(1, 2, 3, 4), 4) shouldBe false
      hasMajorityElem(List(1, 2, 3, 1), 4) shouldBe false
    }
  }

  "QuickSort" - {
    import QuickSort.Sorting

    "should sort an array" in {
      val lst: Array[Int] = Array(2, 3, 9, 2, 2, 10, 11, 5)
      val sorting = new Sorting(lst, Some(lst.length))
      sorting.quickSort()
      sorting.array shouldEqual Array(2, 2, 2, 3, 5, 9, 10, 11)
    }
  }

  "CountInversions" - {
    import CountInversions.countInversions

    "should count the number of inversions in an array" in {
      val lst: List[Int] = List(2, 3, 9, 2, 9)
      countInversions(lst, lst.length)._2 shouldEqual 2
    }

    "should have n over 2 inversions when the list is reverse sorted" in {
      val n: Int = 20
      countInversions((n to 1 by -1).toList, n)._2 shouldEqual n * (n - 1) / 2
    }
  }

  "OrganizingLottery" - {
    import OrganizingLottery.numberOfSegmentsContainingPoints

    "should return the number of segments containing points on the real line" - {
      "test case 1" in {
        val segments: Vector[(Int, Int)] = Vector((0, 5), (7, 10))
        val points: List[Int] = List(1, 6, 11)
        val (left, right): (Vector[Int], Vector[Int]) = segments.unzip
        numberOfSegmentsContainingPoints(left, right, segments.length, points) shouldEqual List(1, 0, 0)
      }

      "test case 2" in {
        val segments: Vector[(Int, Int)] = Vector((-10, 10))
        val points: List[Int] = List(-100, 100, 0)
        val (left, right): (Vector[Int], Vector[Int]) = segments.unzip
        numberOfSegmentsContainingPoints(left, right, segments.length, points) shouldEqual List(0, 0, 1)
      }

      "test case 3" in {
        val segments: Vector[(Int, Int)] = Vector((0, 5), (-3, 2), (7, 10))
        val points: List[Int] = List(1, 6)
        val (left, right): (Vector[Int], Vector[Int]) = segments.unzip
        numberOfSegmentsContainingPoints(left, right, segments.length, points) shouldEqual List(2, 0)
      }

      "test case 4" in {
        val segments: Vector[(Int, Int)] = Vector((0, 4), (-2, 2), (-1, 0), (0, 5), (6, 8))
        val points: List[Int] = List(1, 4)
        val (left, right): (Vector[Int], Vector[Int]) = segments.unzip
        numberOfSegmentsContainingPoints(left, right, segments.length, points) shouldEqual List(3, 2)
      }
    }
  }

  "ClosestPairOfPoints" - {
    import ClosestPairOfPoints.{Point, distanceOfClosestPairOfPoints}

    "should calculate the distance of the closest pair of points" - {
      val absoluteTolerance: Double = 1e-4

      "test case 1" in {
        val points: Vector[Point] = Vector(Point(0, 0), Point(3, 4))
        distanceOfClosestPairOfPoints(points, points.length) shouldBe (5.0 +- absoluteTolerance)
      }

      "test case 2" in {
        val points: Vector[Point] = Vector(Point(7, 7), Point(1, 100), Point(4, 8), Point(7, 7))
        distanceOfClosestPairOfPoints(points, points.length) shouldBe (0.0 +- absoluteTolerance)
      }

      "test case 3" in {
        val points: Vector[Point] =
          Vector(
            Point(4, 4), Point(-2, -2), Point(-3, -4), Point(-1, 3), Point(2, 3), Point(-4, 0), Point(1, 1),
            Point(-1, -1), Point(3, -1), Point(-4, 2), Point(-2, 4)
          )
        distanceOfClosestPairOfPoints(points, points.length) shouldBe (1.414213 +- absoluteTolerance)
      }
    }
  }
}
