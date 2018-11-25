package binary_search_trees

object BinaryTreeTraversals {
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

    def preorderTraversal(): List[Int] = {
      @tailrec
      def loop(keys: List[Int], nodeIndex: Int, stack: Stack[Int]): List[Int] = {
        if (nodeIndex != -1) {
          val Node(key, leftIx, rightIx) = nodes(nodeIndex)
          loop(key :: keys, leftIx, rightIx :: stack)
        }
        else stack match {
          case Nil => keys.reverse
          case ix :: rest => loop(keys, ix, rest)
        }
      }
      loop(Nil, rootIndex, Nil)
    }

    def postorderTraversal(): List[Int] = {
      @tailrec
      def findNodeOrder(stack1: Stack[Int], stack2: Stack[Int]): Stack[Int] = stack1 match {
        case Nil => stack2
        case ix :: rest =>
          if (ix == -1) findNodeOrder(rest, stack2)
          else {
            val Node(_, leftIx, rightIx) = nodes(ix)
            findNodeOrder(rightIx :: leftIx :: rest, ix :: stack2)
          }
      }
      val orderedIndices: List[Int] = findNodeOrder(List(rootIndex), Nil)
      orderedIndices.map(nodes(_).key)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private[binary_search_trees] def readNodeInformation(reader: Iterator[String], nrNodes: Int): Vector[Node] = {
    (for { _ <- 0 until nrNodes } yield {
      val List(key, left, right): List[Int] = convertToIntList(reader.next())
      Node(key, left, right)
    }).toVector
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrNodes: Int = reader.next().toInt
    val nodes: Vector[Node] = readNodeInformation(reader, nrNodes)
    val tree = new BinaryTree(nodes)
    val inorder: List[Int] = tree.inorderTraversal()
    val preorder: List[Int] = tree.preorderTraversal()
    val postorder: List[Int] = tree.postorderTraversal()
    println(inorder.mkString(" "))
    println(preorder.mkString(" "))
    println(postorder.mkString(" "))
  }
}
