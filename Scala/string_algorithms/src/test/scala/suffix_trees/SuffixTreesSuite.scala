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
}
