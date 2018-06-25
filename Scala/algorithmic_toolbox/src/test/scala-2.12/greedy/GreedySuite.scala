package greedy

import org.scalatest.{FreeSpec, Matchers}

class GreedySuite extends FreeSpec with Matchers {

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

}
