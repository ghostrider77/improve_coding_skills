package suffix_trees

object GeneralizedPatternMatching {
  import scala.annotation.tailrec

  private type Node = Int

  final case class EdgeEndpoint(tip: Node, label: Char)

  private val stringTerminator: Char = '$'

  class Trie(words: List[String]) {
    import Trie.getNeighbourWithGivenLabel

    private val nodeCounter: Iterator[Int] = Iterator.from(0)
    private val root: Int = nodeCounter.next()
    private val adjacencyList: Map[Node, List[EdgeEndpoint]] = addWordsToTrie(this.words)

    def patternStarts(text: List[Char]): Boolean = {
      @tailrec
      def loop(ls: List[Char], currentNode: Node): Boolean = ls match {
        case Nil => false
        case letter :: lss => getNeighbourWithGivenLabel(adjacencyList, currentNode, letter) match {
          case None => false
          case Some(nextNode) => if (hasLeafChild(nextNode)) true else loop(lss, nextNode)
        }
      }

      loop(text, root)
    }

    private def hasLeafChild(node: Node): Boolean =
      adjacencyList.getOrElse(node, Nil).exists{ case EdgeEndpoint(_, label) => label == stringTerminator }

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

  private[suffix_trees] def appendTerminator(string: String): String = string + stringTerminator.toString

  def multiplePatternMatching(text: String, patterns: List[String]): List[Int] = {
    val trie = new Trie(patterns)
    @tailrec
    def loop(indices: List[Int], text: List[Char], ix: Int): List[Int] = text match {
      case Nil => indices.reverse
      case _ :: rest =>
        val updatedIndices: List[Int] = if (trie.patternStarts(text)) ix :: indices else indices
        loop(updatedIndices, rest, ix + 1)
    }

    loop(Nil, text.toList, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val nrPatterns: Int = reader.next().toInt
    val patterns: List[String] = reader.take(nrPatterns).map(appendTerminator).toList
    val result: List[Int] = multiplePatternMatching(text, patterns)
    println(result.mkString(" "))
  }
}
