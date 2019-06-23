package burrows_wheeler

import org.scalatest.{FreeSpec, Matchers}

class BurrowsWheelerSuite extends FreeSpec with Matchers {

  "BurrowsWheeler" - {
    import BurrowsWheeler.calcBurrowsWheelerTransform

    "should calculate the Burrows-Wheeler transform of a string" in {
      val texts: List[String] = List("AA$", "ACACACAC$", "AGACATA$")
      texts.map(calcBurrowsWheelerTransform) shouldEqual List("AA$", "CCCC$AAAA", "ATG$CAAA")
    }
  }

  "InverseBurrowsWheeler" - {
    import InverseBurrowsWheeler.inverseBurrowsWheelerTransform

    "should calculate the original string from it Burrows-Wheeler transform" in {
      val transformedStrings: List[String] = List("AC$A", "AGGGAA$")
      transformedStrings.map(_.toList).map(inverseBurrowsWheelerTransform) shouldEqual List("ACA$", "GAGAGA$")
    }
  }

  "MatchCompressedString" - {
    import MatchCompressedString.improvedBWPatternMatching

    "should calculate the number of times each pattern appears in a text given by its Burrows-Wheeler transform" - {
      "test case 1" in {
        val transformedString: String = "AGGGAA$"
        val patterns: List[String] = List("GA")
        improvedBWPatternMatching(transformedString.toVector, patterns) shouldEqual List(3)
      }
      "test case 2" in {
        val transformedString: String = "ATT$AA"
        val patterns: List[String] = List("ATA", "A")
        improvedBWPatternMatching(transformedString.toVector, patterns) shouldEqual List(2, 3)
      }
      "test case 3" in {
        val transformedString: String = "AT$TCTATG"
        val patterns: List[String] = List("TCT", "TATG")
        improvedBWPatternMatching(transformedString.toVector, patterns) shouldEqual List(0, 0)
      }
    }
  }

  "SuffixArray" - {
    import SuffixArray.calcSuffixArray

    "should calculate the suffix array of a string" in {
      val texts: List[String] = List("GAC$", "GAGAGAGA$", "AACGATAGCGGTAGA$")
      texts.map(calcSuffixArray) shouldEqual
        List(
          List(3, 1, 2, 0),
          List(8, 7, 5, 3, 1, 6, 4, 2, 0),
          List(15, 14, 0, 1, 12, 6, 4, 2, 8, 13, 3, 7, 9, 10, 11, 5)
        )
    }
  }
}
