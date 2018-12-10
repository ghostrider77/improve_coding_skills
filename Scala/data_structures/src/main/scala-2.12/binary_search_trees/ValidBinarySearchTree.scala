package binary_search_trees

object ValidBinarySearchTree {
  import scala.annotation.tailrec

  type Stack[T] = List[T]

  final case class Node(key: Int, leftIx: Int, rightIx: Int)

  class BinaryTree(nodes: Vector[Node]) {
    private val rootIndex: Int = 0

    def inorderTraversal(): List[Int] = {
      @tailrec
      def loop(keys: List[Int], nodeIndex: Int, stack: Stack[Node]): List[Int] = {
        if (nodeIndex != -1) {
          val node: Node = nodes(nodeIndex)
          loop(keys, node.leftIx, node :: stack)
        }
        else stack match {
          case Nil => keys.reverse
          case Node(key, _, rightIx) :: rest => loop(key :: keys, rightIx, rest)
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

  private def isSorted(lst: List[Int], length: Int): Boolean =
    length <= 1 || lst.sliding(2).forall { case List(a, b) => a <= b }

  def isValidBinarySearchTree(tree: BinaryTree, nrNodes: Int): Boolean = {
    if (nrNodes <= 1) true
    else {
      val inorderKeys: List[Int] = tree.inorderTraversal()
      isSorted(inorderKeys, nrNodes)
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
