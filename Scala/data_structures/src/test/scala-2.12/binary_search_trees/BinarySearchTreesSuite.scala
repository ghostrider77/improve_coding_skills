package binary_search_trees

import org.scalatest.{FreeSpec, Matchers}

class BinarySearchTreesSuite extends FreeSpec with Matchers {

  "BinaryTreeTraversals" - {
    import BinaryTreeTraversals.{BinaryTree, Node, readNodeInformation}

    "should perform the 3 kind of binary tree traversals" - {
      "test case 1" in {
        val nrNodes: Int = 5
        val lines: Iterator[String] =
          List(
            "4 1 2",
            "2 3 4",
            "5 -1 -1",
            "1 -1 -1",
            "3 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        tree.inorderTraversal() shouldEqual List(1, 2, 3, 4, 5)
        tree.preorderTraversal() shouldEqual List(4, 2, 1, 3, 5)
        tree.postorderTraversal() shouldEqual List(1, 3, 2, 5, 4)
      }

      "test case 2" in {
        val nrNodes: Int = 10
        val lines: Iterator[String] =
          List(
            "0 7 2",
            "10 -1 -1",
            "20 -1 6",
            "30 8 9",
            "40 3 -1",
            "50 -1 -1",
            "60 1 -1",
            "70 5 4",
            "80 -1 -1",
            "90 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        tree.inorderTraversal() shouldEqual List(50, 70, 80, 30, 90, 40, 0, 20, 10, 60)
        tree.preorderTraversal() shouldEqual List(0, 70, 50, 40, 30, 80, 90, 20, 60, 10)
        tree.postorderTraversal() shouldEqual List(50, 80, 90, 30, 40, 70, 10, 60, 20, 0)
      }
    }
  }

  "ValidBinarySearchTree" - {
    import ValidBinarySearchTree.{BinaryTree, Node, isValidBinarySearchTree, readNodeInformation}

    "should detect if the binary tree is a binary search tree" - {
      "test case 1" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("2 1 2", "1 -1 -1", "3 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 2" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("1 1 2", "2 -1 -1", "3 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe false
      }

      "test case 3" in {
        val nrNodes: Int = 0
        val lines: Iterator[String] = List().toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 4" in {
        val nrNodes: Int = 5
        val lines: Iterator[String] =
          List(
            "1 -1 1",
            "2 -1 2",
            "3 -1 3",
            "4 -1 4",
            "5 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 5" in {
        val nrNodes: Int = 7
        val lines: Iterator[String] =
          List(
            "4 1 2",
            "2 3 4",
            "6 5 6",
            "1 -1 -1",
            "3 -1 -1",
            "5 -1 -1",
            "7 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 6" in {
        val nrNodes: Int = 4
        val lines: Iterator[String] =
          List(
            "4 1 -1",
            "2 2 3",
            "1 -1 -1",
            "5 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe false
      }
    }
  }

  "ValidBinarySearchTreeHard" - {
    import ValidBinarySearchTreeHard.{BinaryTree, Node, isValidBinarySearchTree, readNodeInformation}

    "should detect if the binary tree is a binary search tree" - {
      "test case 1" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("2 1 2", "1 -1 -1", "3 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 2" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("1 1 2", "2 -1 -1", "3 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe false
      }

      "test case 3" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("2 1 2", "1 -1 -1", "2 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 4" in {
        val nrNodes: Int = 3
        val lines: Iterator[String] = List("2 1 2", "2 -1 -1", "3 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe false
      }

      "test case 5" in {
        val nrNodes: Int = 0
        val lines: Iterator[String] = List().toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 6" in {
        val nrNodes: Int = 1
        val lines: Iterator[String] = List("2147483647 -1 -1").toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 7" in {
        val nrNodes: Int = 5
        val lines: Iterator[String] =
          List(
            "1 -1 1",
            "2 -1 2",
            "3 -1 3",
            "4 -1 4",
            "5 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }

      "test case 8" in {
        val nrNodes: Int = 7
        val lines: Iterator[String] =
          List(
            "4 1 2",
            "2 3 4",
            "6 5 6",
            "1 -1 -1",
            "3 -1 -1",
            "5 -1 -1",
            "7 -1 -1"
          ).toIterator
        val nodes: Vector[Node] = readNodeInformation(lines, nrNodes)
        val tree = new BinaryTree(nodes)
        isValidBinarySearchTree(tree, nrNodes) shouldBe true
      }
    }
  }
}
