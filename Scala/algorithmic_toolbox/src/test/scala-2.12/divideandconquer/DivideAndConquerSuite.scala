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
}
