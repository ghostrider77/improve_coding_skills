package divideandconquer

import scala.annotation.tailrec
import scala.util.Random

object QuickSort {

  class Sorting(val array: Array[Int], n: Option[Int]) {
    type Index = Int

    private val length: Int = n.getOrElse(array.length)
    private val rng = new Random(2112L)

    private[this] def swapElems(ix: Index, elem: Int, jy: Index): Unit = {
      array(ix) = array(jy)
      array(jy) = elem
    }

    private[this] def threeWayPartitioning(pivot: Int, start: Index, end: Index): (Index, Index) = {
      @tailrec
      def loop(ix: Index, middleStart: Index, middleEnd: Int): (Index, Index) = {
        if (ix > middleEnd) (middleStart, middleEnd)
        else {
          val elem: Int = array(ix)
          if (elem < pivot) {
            if (ix != middleStart) swapElems(ix, elem, middleStart)
            loop(ix + 1, middleStart + 1, middleEnd)
          } else if (elem > pivot) {
            swapElems(ix, elem, middleEnd)
            loop(ix, middleStart, middleEnd - 1)
          } else loop(ix + 1, middleStart, middleEnd)
        }
      }
      loop(ix = start, middleStart = start, middleEnd = end)
    }

    def quickSort(): Unit = {
      @tailrec
      def loop(stack: List[(Index, Index)]): Unit = {
        if (stack.nonEmpty) {
          val (leftEnd, rightEnd): (Index, Index) = stack.head
          if (leftEnd >= rightEnd) loop(stack.tail)
          else {
            val randomIx: Index = leftEnd + rng.nextInt(rightEnd - leftEnd + 1)
            val pivot: Int = array(randomIx)
            val (middleStart, middleEnd): (Index, Index) = threeWayPartitioning(pivot, leftEnd, rightEnd)
            loop((middleEnd + 1, rightEnd) :: (leftEnd, middleStart - 1) :: stack.tail)
          }
        }
      }
      loop(stack = List((0, length - 1)))
    }
  }

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val lst: Array[Int] = convertToIntArray(reader.next())
    val sorting = new Sorting(lst, Some(n))
    sorting.quickSort()
    println(sorting.array.mkString(" "))
  }
}
