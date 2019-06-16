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
}
