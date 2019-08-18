package challenges

import org.scalatest.{FreeSpec, Matchers}

class ChallengesSuite extends FreeSpec with Matchers {

  "PatternOccurrences" - {
    import PatternOccurrences.patternOccurrencesInGenome

    "should return the indices where pattern appears in genome" - {
      "test case 1" in {
        val pattern: String = "TACG"
        val genome: String = "GT"
        patternOccurrencesInGenome(genome, pattern) shouldEqual Nil
      }

      "test case 2" in {
        val pattern: String = "ATA"
        val genome: String = "ATATA"
        patternOccurrencesInGenome(genome, pattern) shouldEqual List(0, 2)
      }

      "test case 3" in {
        val pattern: String = "ATAT"
        val genome: String = "GATATATGCATATACTT"
        patternOccurrencesInGenome(genome, pattern) shouldEqual List(1, 3, 9)
      }
    }
  }

  "ImprovedSuffixArray" - {
    import ImprovedSuffixArray.calcSuffixArray

    "should calculate the suffix array of a larger string" in {
      val texts: List[String] = List("AAA$", "GAC$", "GAGAGAGA$", "AACGATAGCGGTAGA$")
      texts.map(calcSuffixArray) shouldEqual
        List(
          List(3, 2, 1, 0),
          List(3, 1, 2, 0),
          List(8, 7, 5, 3, 1, 6, 4, 2, 0),
          List(15, 14, 0, 1, 12, 6, 4, 2, 8, 13, 3, 7, 9, 10, 11, 5)
        )
    }
  }
}
