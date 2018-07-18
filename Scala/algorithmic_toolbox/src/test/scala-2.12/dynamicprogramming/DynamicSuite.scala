package dynamicprogramming

import org.scalatest.{FreeSpec, Matchers}

class DynamicSuite extends FreeSpec with Matchers {

  "PrimitiveCalculator" - {
    import PrimitiveCalculator.runCalculator

    def calculatorValidator(steps: List[Int], n: Int): Boolean =
      steps.head == 1 && steps.last == n &&
        steps.sliding(2).forall { case List(k, m) => Set(k + 1, 2 * k, 3 * k).contains(m) }

    "should return the minimum number of allowed operations needed to obtain n starting from 1" in {
      runCalculator(1) shouldEqual List(1)

      val n1: Int = 5
      val result1: List[Int] = runCalculator(n1)
      result1 should have length 4
      calculatorValidator(result1, n1) shouldBe true

      val n2: Int = 96234
      val result2: List[Int] = runCalculator(n2)
      result2 should have length 15
      calculatorValidator(result2, n2) shouldBe true
    }
  }

  "Knapsack" - {
    import Knapsack.solveKnapsackProblem

    "should solve the knapsack without repetitions problem" in {
      val capacity: Int = 10
      val weights: List[Int] = List(1, 4, 8)
      solveKnapsackProblem(weights, weights.length, capacity) shouldEqual 9
    }
  }

  "EditDistance" - {
    import EditDistance.calcLevenshteinDistance

    "should calculate the edit distance of two strings" in {
      calcLevenshteinDistance("ab", "ab") shouldEqual 0
      calcLevenshteinDistance("short", "ports") shouldEqual 3
      calcLevenshteinDistance("editing", "distance") shouldEqual 5
    }
  }
}
