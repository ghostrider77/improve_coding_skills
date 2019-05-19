package suffix_trees

object TrieConstruction {
  import scala.annotation.tailrec

  private type Node = Int

  final case class EdgeEndpoint(tip: Node, label: Char)

  class Trie(words: List[String]) {
    import Trie.getNeighbourWithGivenLabel

    private val nodeCounter: Iterator[Int] = Iterator.from(0)
    private val root: Int = nodeCounter.next()
    private val adjacencyList: Map[Node, List[EdgeEndpoint]] = addWordsToTrie(this.words)

    def stringRepresentation: List[String] = {
      (for {
        (node, neighbours) <- adjacencyList
        EdgeEndpoint(neighbour, label) <- neighbours
      } yield s"$node->$neighbour:$label").toList
    }

    private def addWordsToTrie(words: List[String]): Map[Node, List[EdgeEndpoint]] = {
      @tailrec
      def loop(ws: List[String], trie: Map[Node, List[EdgeEndpoint]]): Map[Node, List[EdgeEndpoint]] = ws match {
        case Nil => trie
        case w :: wss =>
          val updatedTrie: Map[Node, List[EdgeEndpoint]] = addWord(trie, w)
          loop(wss, updatedTrie)
      }

      loop(words, Map())
    }

    private def addWord(trie: Map[Node, List[EdgeEndpoint]], w: String): Map[Node, List[EdgeEndpoint]] = {
      @tailrec
      def loop(ls: List[Char],
               acc: Map[Node, List[EdgeEndpoint]],
               currentNode: Node): Map[Node, List[EdgeEndpoint]] = ls match {
        case Nil => acc
        case letter :: lss => getNeighbourWithGivenLabel(acc, currentNode, letter) match {
          case None =>
            val nextNode: Node = nodeCounter.next()
            val updatedTrie: Map[Node, List[EdgeEndpoint]] =
              acc.updated(currentNode, EdgeEndpoint(nextNode, letter) :: acc.getOrElse(currentNode, Nil))
            loop(lss, updatedTrie, nextNode)
          case Some(node) => loop(lss, acc, node)
          }
      }

      loop(w.toList, trie, root)
    }
  }

  object Trie {
    private def getNeighbourWithGivenLabel(trie: Map[Node, List[EdgeEndpoint]],
                                           currentNode: Node,
                                           letter: Char): Option[Node] = {
      trie
        .get(currentNode)
        .flatMap(_.find{ case EdgeEndpoint(_, label) => label == letter })
        .map(_.tip)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrPatterns: Int = reader.next().toInt
    val t0 = System.nanoTime()
    val patterns: List[String] = reader.take(nrPatterns).toList
    val trie = new Trie(patterns)
    trie.stringRepresentation.foreach(println)
  }
}
