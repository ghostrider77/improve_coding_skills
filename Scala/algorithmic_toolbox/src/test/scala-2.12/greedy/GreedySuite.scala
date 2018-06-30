package greedy

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class GreedySuite extends FreeSpec with Matchers with Inspectors {
  object Constants {
    val absoluteTolerance: Double = 1e-3
  }

  "ChangingMoney" - {
    import ChangingMoney.calcMinimumNumberOfCoins

    "should calculate the minimum number of coins that change the given amount" in {
      calcMinimumNumberOfCoins(1) shouldEqual 1
      calcMinimumNumberOfCoins(2) shouldEqual 2
      calcMinimumNumberOfCoins(9) shouldEqual 5
      calcMinimumNumberOfCoins(10) shouldEqual 1
      calcMinimumNumberOfCoins(28) shouldEqual 6
    }
  }

  "FractionalKnapsack" - {
    import FractionalKnapsack.{solveFractionalKnapsack, Item}
    import Constants.absoluteTolerance

    "should solve the fractional knapsapck problem when any fraction of an item can be put into the knapsack" in {
      val items: List[Item] =
        List(
          Item(value = 60, weight = 20),
          Item(value = 100, weight = 50),
          Item(value = 120, weight = 30)
        )
      val capacity: Int = 50
      solveFractionalKnapsack(items, capacity) shouldBe (180.0 +- absoluteTolerance)
    }

    "should select the best item only when its weight exceeds the capacity" in {
      val items: List[Item] =
        List(
          Item(value = 60, weight = 20),
          Item(value = 120, weight = 30)
        )
      val capacity: Int = 20
      solveFractionalKnapsack(items, capacity) shouldBe (80.0 +- absoluteTolerance)
    }

    "should use the only item available up to the given capacity of the knapsack" in {
      val singleItem: List[Item] = List(Item(value = 20, weight = 10))
      solveFractionalKnapsack(singleItem, capacity = 5) shouldBe (10.0 +- absoluteTolerance)
      solveFractionalKnapsack(singleItem, capacity = 20) shouldBe (20.0 +- absoluteTolerance)
    }
  }

  "MaximizingRevenue" - {
    import MaximizingRevenue.calcMaximalRevenue

    "should calculate the maximal inner product of two vectors when one of them can be permutated" in {
      calcMaximalRevenue(List(23), List(39)) shouldEqual 897

      val lst1: List[Int] = List(1, 3, -5)
      val lst2: List[Int] = List(-2, 4, 1)
      calcMaximalRevenue(lst1, lst2) shouldEqual 23
    }

    "should calculate maximal revenue when the product exceed the integer range" in {
      val lst1: List[Int] = List(100000, 10000)
      val lst2: List[Int] = List(10000, 100000)
      calcMaximalRevenue(lst1, lst2) shouldEqual 10100000000L
    }
  }

  "CollectingSignatures" - {
    import CollectingSignatures.{calcMinimumNumberOfPointsCoveringSegments, Segment}

    "should calculate the minimum number of integer points such that each segment contains at least one of them" - {

      "test case 1" in {
        val segments: List[Segment] = List(Segment(1, 3), Segment(2, 5), Segment(3, 6))
        val points: List[Int] = calcMinimumNumberOfPointsCoveringSegments(segments)
        points.length shouldEqual 1
        forAll(segments) { case Segment(x, y) =>
          points.exists(p => x <= p && p <= y)
        }
      }

      "test case 2" in {
        val segments: List[Segment] = List(Segment(4, 7), Segment(1, 3), Segment(2, 5), Segment(5, 6))
        val points: List[Int] = calcMinimumNumberOfPointsCoveringSegments(segments)
        points.length shouldEqual 2
        forAll(segments) { case Segment(x, y) =>
          points.exists(p => x <= p && p <= y)
        }
      }

      "test case 3" in {
        val n: Int = 10
        val segments: List[Segment] = (for { ix <- 1 to n } yield Segment(0, ix)).toList
        val points: List[Int] = calcMinimumNumberOfPointsCoveringSegments(segments)
        points.length shouldEqual 1
        forAll(segments) { case Segment(x, y) =>
          points.exists(p => x <= p && p <= y)
        }
      }

      "test case 4" in {
        val n: Int = 10
        val segments: List[Segment] = (for { ix <- 1 to 2*n by 2 } yield Segment(ix, ix + 1)).toList
        val points: List[Int] = calcMinimumNumberOfPointsCoveringSegments(segments)
        points.length shouldEqual n
        forAll(segments) { case Segment(x, y) =>
          points.exists(p => x <= p && p <= y)
        }
      }
    }
  }

  "DistinctDecomposition" - {
    import DistinctDecomposition.decomposeToDistinctElems

    "should calculate the maximum number of distinct elems such that their sum equals to a given integer" - {
      "test case 1" in {
        val n: Int = 6
        val summands: List[Int] = decomposeToDistinctElems(n)
        summands.length shouldEqual 3
        summands.sum shouldEqual n
        summands.toSet should have size 3
      }

      "test case 2" in {
        val n: Int = 8
        val summands: List[Int] = decomposeToDistinctElems(n)
        summands.length shouldEqual 3
        summands.sum shouldEqual n
        summands.toSet should have size 3
      }

      "test case 3" in {
        val n: Int = 2
        val summands: List[Int] = decomposeToDistinctElems(n)
        summands.length shouldEqual 1
        summands.sum shouldEqual n
        summands.toSet should have size 1
      }
    }
  }

  "MaximizingSalary" - {
    import MaximizingSalary.assembleLargestNumberFromPieces

    "should calculate the largest integer that can be assembled from the given pieces" in {
      assembleLargestNumberFromPieces(List("21", "2")) shouldEqual "221"
      assembleLargestNumberFromPieces(List("9", "4", "6", "1", "9")) shouldEqual "99641"
      assembleLargestNumberFromPieces(List("23", "39", "92")) shouldEqual "923923"
      assembleLargestNumberFromPieces(List("243", "24", "245")) shouldEqual "24524324"
    }
  }
}
