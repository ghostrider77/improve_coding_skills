package divideandconquer

import org.scalatest.{FreeSpec, Matchers}

class DivideAndConquerSuite extends FreeSpec with Matchers {

  "BinarySearch" - {
    import BinarySearch.findElems

    "should find the index of a given number in a sorted list" in {
      val lst: List[Int] = List(2, 5, 6, 7, 10, 12, 20)
      val n: Int = lst.length
      val queries: List[Int] = List(1, 5, 8, 10, 15, 20, 25)
      findElems(lst, n, queries) shouldEqual List(-1, 1, -1, 4, -1, 6, -1)
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
}
