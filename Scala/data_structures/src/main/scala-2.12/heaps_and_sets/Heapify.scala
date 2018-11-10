package heaps_and_sets

object Heapify {
  import scala.annotation.tailrec
  type IndexPair = (Int, Int)

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def getIndexOfParentChildrenMinimum(array: Array[Int], parentIndex: Int, size: Int): Int = {
    val leftChildIx: Int = 2 * parentIndex + 1
    val rightChildIx: Int = leftChildIx + 1
    val minIndex: Int = if (leftChildIx < size &&  array(leftChildIx) < array(parentIndex)) leftChildIx else parentIndex
    if (rightChildIx < size && array(rightChildIx) < array(minIndex)) rightChildIx else minIndex
  }

  private[heaps_and_sets] def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = array(ix)
    array(ix) = array(jy)
    array(jy) = elem
  }

  private def siftDown(array: Array[Int], parentIndex: Int, n: Int): List[IndexPair] = {
    @tailrec
    def loop(currentParentIx: Int, currentMinIx: Int, swaps: List[IndexPair]): List[IndexPair] = {
      if (currentMinIx == currentParentIx) swaps
      else {
        swapElems(array, currentMinIx, currentParentIx)
        val nextParentIx: Int = currentMinIx
        val nextMinIx: Int = getIndexOfParentChildrenMinimum(array, nextParentIx, n)
        loop(nextParentIx, nextMinIx, (currentParentIx, currentMinIx) :: swaps)
      }
    }
    val minIndex: Int = getIndexOfParentChildrenMinimum(array, parentIndex, n)
    loop(parentIndex, minIndex, Nil)
  }

  def heapify(array: Array[Int], n: Int): List[IndexPair] = {
    @tailrec
    def loop(swaps: List[IndexPair], parentIndex: Int): List[IndexPair] = {
      if (parentIndex < 0) swaps.reverse
      else {
        val currentSwaps: List[IndexPair] = siftDown(array, parentIndex, n)
        loop(currentSwaps ::: swaps, parentIndex - 1)
      }
    }
    loop(Nil, n / 2 - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    val swaps: List[IndexPair] = heapify(array, n)
    println(swaps.length)
    swaps.foreach{ case (i, j) => println(s"$i $j")}
  }
}
