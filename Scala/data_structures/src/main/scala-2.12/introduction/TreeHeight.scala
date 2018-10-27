package introduction

object TreeHeight {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  final case class Node(key: Int, children: Set[Int])

  class Tree(nrNodes: Int, parentsOfNodes: List[Int]) {
    private lazy val (root, nodes): (Node, Vector[Node]) = buildTree()

    private def buildTree(): (Node, Vector[Node]) = {
      val childrenOfNodes: Map[Int, Set[Int]] =
        parentsOfNodes
          .zipWithIndex
          .groupBy { case (parentId, _) => parentId }
          .mapValues(_.map { case (_, nodeId) => nodeId }.toSet)
      val nodes: Vector[Node] =
        (for { k <- 0 until nrNodes } yield Node(k, childrenOfNodes.getOrElse(k, Set()))).toVector
      val indexOfRoot: Int = childrenOfNodes(-1).head
      (nodes(indexOfRoot), nodes)
    }

    private def getChildrenOfNodes(keys: Set[Int]): Set[Int] = keys.flatMap(nodes(_).children)

    def calcDepth(): Int = {
      @tailrec
      def loop(depth: Int, keys: Set[Int]): Int = {
        if (keys.isEmpty) depth
        else {
          val nextKeys: Set[Int] = getChildrenOfNodes(keys)
          loop(depth + 1, nextKeys)
        }
      }
      loop(0, Set(root.key))
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrNodes: Int = reader.next().toInt
    val parentsOfNodes: List[Int] = convertToIntList(reader.next())
    val tree = new Tree(nrNodes, parentsOfNodes)
    val depth: Int = tree.calcDepth()
    println(depth)
  }
}
