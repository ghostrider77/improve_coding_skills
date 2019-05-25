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
}
