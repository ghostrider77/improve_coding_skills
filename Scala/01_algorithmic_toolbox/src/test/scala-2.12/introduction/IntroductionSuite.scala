package introduction

import org.scalatest.{FreeSpec, Matchers}

class IntroductionSuite extends FreeSpec with Matchers {

  "SmallFibonacciNumber" - {
    import SmallFibonacciNumber.calcFibonacciNumber

    "should calculate Fibonacci numbers not exceeding the largest integer value" in {
      calcFibonacciNumber(0) shouldEqual 0
      calcFibonacciNumber(1) shouldEqual 1

      calcFibonacciNumber(3) shouldEqual 2
      calcFibonacciNumber(10) shouldEqual 55
    }
  }

  "LastDigitOfFibonacci" - {
    import LastDigitOfFibonacci.calcFibonacciLastDigit

    "should calculate the last digit of a large Fibonacci number" in {
      calcFibonacciLastDigit(3) shouldEqual 2
      calcFibonacciLastDigit(331) shouldEqual 9
      calcFibonacciLastDigit(327305) shouldEqual 5
    }
  }

  "GreatestCommonDivisor" - {
    import GreatestCommonDivisor.calcGCD

    "should calculate the greatest common divisor of 2 positive integers" in {
      calcGCD(18, 35) shouldEqual 1
      calcGCD(35, 18) shouldEqual 1
      calcGCD(64, 16) shouldEqual 16
      calcGCD(28851538, 1183019) shouldEqual 17657
    }
  }

  "LeastCommonMultiple" - {
    import LeastCommonMultiple.calcLCM

    "should calculate the least common multiple of 2 positive integers" in {
      calcLCM(6, 8) shouldEqual 24
      calcLCM(10, 11) shouldEqual 110
      calcLCM(28851538, 1183019) shouldEqual 1933053046

      val largeInt: Int = 2e9.toInt
      calcLCM(largeInt, largeInt + 1) shouldEqual largeInt.toLong * (largeInt + 1)
    }
  }

}
