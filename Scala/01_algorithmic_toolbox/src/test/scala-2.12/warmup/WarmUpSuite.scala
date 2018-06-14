package warmup

import org.scalatest.{FreeSpec, Matchers}

class WarmUpSuite extends FreeSpec with Matchers {

  "APlusB" - {
    import APlusB.addTwoNumbers

    "should add two integers" in {
      val a: Int = 2
      val b: Int = 3
      addTwoNumbers(a, b) shouldEqual 5
    }
  }

  "MaximumPairwiseProduct" - {
    import MaximumPairwiseProduct.calcMaximumPairwiseProduct
    import scala.util.Random

    "should calculate the product of the largest and the second largest element in a list" in {
      val lst1: List[Int] = List(4, 5, 1)
      val lst2: List[Int] = List(1, 2, 10, 3, 10, 9)

      calcMaximumPairwiseProduct(lst1) shouldEqual 20
      calcMaximumPairwiseProduct(lst2) shouldEqual 100
    }

    "should calculate the product correctly when the maximum values are large" in {
      val lst: List[Int] = List(0, 1, 2, 2e9.toInt, 3, 2e9.toInt, 4)
      calcMaximumPairwiseProduct(lst) shouldEqual 4e18.toLong
    }
  }

}
