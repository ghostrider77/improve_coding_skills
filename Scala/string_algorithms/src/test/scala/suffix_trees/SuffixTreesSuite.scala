package suffix_trees

import org.scalatest.{FreeSpec, Matchers}

class SuffixTreesSuite extends FreeSpec with Matchers {

  "TrieConstruction" - {
    import TrieConstruction.Trie

    "should create a trie from a list of words" - {
      "test case 1" in {
        val patterns: List[String] = List("ATA")
        val trie = new Trie(patterns)
        trie.stringRepresentation.toSet shouldEqual Set("0->1:A", "2->3:A", "1->2:T")
      }

      "test case 2" in {
        val patterns: List[String] = List("AT", "AG", "AC")
        val trie = new Trie(patterns)
        trie.stringRepresentation.toSet shouldEqual Set("0->1:A", "1->4:C", "1->3:G", "1->2:T")
      }

      "test case 3" in {
        val patterns: List[String] = List("ATAGA", "ATC", "GAT")
        val trie = new Trie(patterns)
        trie.stringRepresentation.toSet shouldEqual
          Set("0->1:A", "1->2:T", "2->3:A", "3->4:G", "4->5:A", "2->6:C", "0->7:G", "7->8:A", "8->9:T")
      }
    }
  }

  "MultiplePatternMatching" - {
    import MultiplePatternMatching.multiplePatternMatching

    "should return the list of indices where a pattern appears as substring in text" - {
      "test case 1" in {
        val text: String = "AAA"
        val patterns: List[String] = List("AA")
        multiplePatternMatching(text, patterns) shouldEqual List(0, 1)
      }

      "test case 2" in {
        val text: String = "AA"
        val patterns: List[String] = List("T")
        multiplePatternMatching(text, patterns) shouldBe empty
      }

      "test case 3" in {
        val text: String = "AATCGGGTTCAATCGGGGT"
        val patterns: List[String] = List("ATCG", "GGGT")
        multiplePatternMatching(text, patterns) shouldEqual List(1, 4, 11, 15)
      }
    }
  }

  "GeneralizedPatternMatching" - {
    import GeneralizedPatternMatching.{appendTerminator, multiplePatternMatching}

    "should return the indices where a pattern appears as substring in text where a pattern can be a " +
      "prefix of another pattern" - {
      "test case 1" in {
        val text: String = "AAA"
        val patterns: List[String] = List("AA").map(appendTerminator)
        multiplePatternMatching(text, patterns) shouldEqual List(0, 1)
      }

      "test case 2" in {
        val text: String = "ACATA"
        val patterns: List[String] = List("AT", "A", "AG").map(appendTerminator)
        multiplePatternMatching(text, patterns) shouldEqual List(0, 2, 4)
      }

      "test case 3" in {
        val text: String = "BABCAD"
        val patterns: List[String] = List("ABCA", "BCA", "ABD", "AB").map(appendTerminator)
        multiplePatternMatching(text, patterns) shouldEqual List(1, 2)
      }
    }
  }
}
