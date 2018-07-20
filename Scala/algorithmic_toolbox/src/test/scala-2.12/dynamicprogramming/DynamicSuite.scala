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

  "ArithmeticExpression" - {
    import ArithmeticExpression.{maximizeExpression, readInputData}

    "should calculate the maximal value of an arithmetic expression when the order of computation is not fixed" - {
      "test case 1" in {
        val line: String = "1+5"
        val (digits, operations): (List[Long], Vector[String]) = readInputData(line)
        maximizeExpression(digits, operations) shouldEqual 6
      }

      "test case 2" in {
        val line: String = "5-8+7*4-8+9"
        val (digits, operations): (List[Long], Vector[String]) = readInputData(line)
        maximizeExpression(digits, operations) shouldEqual 200
      }
    }
  }

  "LongestCommonSubsequence" - {
    import LongestCommonSubsequence.{calcLongestCommonSubsequence, Sequence}

    "should calculate the length of the longest common subsequence of 3 integer vector" - {
      "test case 1" in {
        val sequences: List[Sequence] =
          List(
            Sequence(seq = Vector(1, 2, 3), length = 3),
            Sequence(seq = Vector(2, 1, 3), length = 3),
            Sequence(seq = Vector(1, 3, 5), length = 3)
          )
        calcLongestCommonSubsequence(sequences) shouldEqual 2
      }

      "test case 2" in {
        val sequences: List[Sequence] =
          List(
            Sequence(seq = Vector(8, 3, 2, 1, 7), length = 5),
            Sequence(seq = Vector(8, 2, 1, 3, 8, 10, 7), length = 7),
            Sequence(seq = Vector(6, 8, 3, 1, 4, 7), length = 6)
          )
        calcLongestCommonSubsequence(sequences) shouldEqual 3
      }
    }
  }
}
