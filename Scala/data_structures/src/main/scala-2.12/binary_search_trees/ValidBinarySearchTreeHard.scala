package binary_search_trees

object ValidBinarySearchTreeHard {
  import scala.annotation.tailrec

  type Stack[T] = List[T]

  final case class Node(key: Int, leftIx: Int, rightIx: Int)

  class BinaryTree(val nodes: Vector[Node]) {
    private val rootIndex: Int = 0

    def inorderTraversal(): List[Int] = {
      @tailrec
      def loop(nodeIndices: List[Int], nodeIndex: Int, stack: Stack[Int]): List[Int] = {
        if (nodeIndex != -1) {
          val Node(_, leftIx, _) = nodes(nodeIndex)
          loop(nodeIndices, leftIx, nodeIndex :: stack)
        }
        else stack match {
          case Nil => nodeIndices.reverse
          case ix :: rest =>
            val Node(_, _, rightIx) = nodes(ix)
            loop(ix :: nodeIndices, rightIx, rest)
        }
      }
      loop(Nil, rootIndex, Nil)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private[binary_search_trees] def readNodeInformation(reader: Iterator[String], nrNodes: Int): Vector[Node] = {
    (for { _ <- 0 until nrNodes } yield {
      val List(key, left, right): List[Int] = convertToIntList(reader.next())
      Node(key, left, right)
    }).toVector
  }

  private def isSorted(lst: Vector[Int], length: Int): Boolean =
    length <= 1 || lst.sliding(2).forall { case Vector(a, b) => a <= b }

  private def areDuplicatesInRightSubtree(keys: Vector[Int], indicesOfNodes: List[Int], tree: BinaryTree): Boolean = {
    @tailrec
    def loop(ixs: List[(Int, Int)]): Boolean = ixs match {
      case Nil => true
      case (indexOfNode, ix) :: ixss =>
        val Node(key, leftIx, _) = tree.nodes(indexOfNode)
        if (leftIx == -1) loop(ixss)
        else {
          val keyToTheLeft: Int = keys(ix - 1)
          if (keyToTheLeft == key) false
          else loop(ixss)
        }
    }
    loop(indicesOfNodes.zipWithIndex)
  }

  def isValidBinarySearchTree(tree: BinaryTree, nrNodes: Int): Boolean = {
    if (nrNodes <= 1) true
    else {
      val inorderIndicesOfNodes: List[Int] = tree.inorderTraversal()
      val keys: Vector[Int] = inorderIndicesOfNodes.map(tree.nodes(_).key).toVector
      isSorted(keys, nrNodes) && areDuplicatesInRightSubtree(keys, inorderIndicesOfNodes, tree)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrNodes: Int = reader.next().toInt
    val nodes: Vector[Node] = readNodeInformation(reader, nrNodes)
    val tree = new BinaryTree(nodes)
    val verdict: Boolean = isValidBinarySearchTree(tree, nrNodes)
    println(if (verdict) "CORRECT" else "INCORRECT")
  }
}
