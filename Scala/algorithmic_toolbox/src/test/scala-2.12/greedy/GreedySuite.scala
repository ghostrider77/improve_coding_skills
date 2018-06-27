package greedy

import org.scalatest.{FreeSpec, Matchers}

class GreedySuite extends FreeSpec with Matchers {
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
}
