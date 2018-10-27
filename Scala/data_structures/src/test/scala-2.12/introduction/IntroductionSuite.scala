package introduction

import org.scalatest.{FreeSpec, Matchers}

class IntroductionSuite extends FreeSpec with Matchers {

  "CheckBrackets" - {
    import CheckBrackets.findIndexOfNonMatchingBracket

    "should return the index of the first non-matching bracket" - {
      "test case 1" in {
        val string: String = "[]"
        findIndexOfNonMatchingBracket(string.toList) shouldBe empty
      }

      "test case 2" in {
        val string: String = "{}[]"
        findIndexOfNonMatchingBracket(string.toList) shouldBe empty
      }

      "test case 3" in {
        val string: String = "[({})]"
        findIndexOfNonMatchingBracket(string.toList) shouldBe empty
      }

      "test case 4" in {
        val string: String = "{[]}()"
        findIndexOfNonMatchingBracket(string.toList) shouldBe empty
      }

      "test case 5" in {
        val string: String = "{"
        findIndexOfNonMatchingBracket(string.toList) shouldBe Some(0)
      }

      "test case 6" in {
        val string: String = "{[}"
        findIndexOfNonMatchingBracket(string.toList) shouldBe Some(2)
      }

      "test case 7" in {
        val string: String = "foo(bar);"
        findIndexOfNonMatchingBracket(string.toList) shouldBe empty
      }

      "test case 8" in {
        val string: String = "foo(bar[i);"
        findIndexOfNonMatchingBracket(string.toList) shouldBe Some(9)
      }
    }
  }

  "TreeHeight" - {
    import TreeHeight.Tree

    "should have depth 1 when there is no other node than the root" in {
      val nrNodes: Int = 1
      val parentsOfNodes: List[Int] = List(-1)
      val tree = new Tree(nrNodes, parentsOfNodes)
      tree.calcDepth() shouldEqual 1
    }

    "should calculate the height / depth of a tree given by an array of parentIds" - {
      "test case 1" in {
        val nrNodes: Int = 5
        val parentsOfNodes: List[Int] = List(4, -1, 4, 1, 1)
        val tree = new Tree(nrNodes, parentsOfNodes)
        tree.calcDepth() shouldEqual 3
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val parentsOfNodes: List[Int] = List(-1, 0, 4, 0, 3)
        val tree = new Tree(nrNodes, parentsOfNodes)
        tree.calcDepth() shouldEqual 4
      }

      "test case 3" in {
        val nrNodes: Int = 6
        val parentsOfNodes: List[Int] = List(5, 5, 5, 5, 5, -1)
        val tree = new Tree(nrNodes, parentsOfNodes)
        tree.calcDepth() shouldEqual 2
      }
    }
  }
}
