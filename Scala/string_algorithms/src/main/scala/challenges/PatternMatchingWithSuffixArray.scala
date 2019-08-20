package challenges

object PatternMatchingWithSuffixArray {
  import scala.annotation.tailrec
  import scala.collection.breakOut
  import utils.Utils.RichNumber._

  private val StringTerminator: String = "$"

  case class StringSlice(string: String, start: Int, until: Int) extends Ordered[StringSlice] {
    val length: Int = until - start

    def compare(that: StringSlice): Int = {
      val commonLength: Int = math.min(this.length, that.length)
      @tailrec
      def loop(ix: Int): Int = {
        if (ix == commonLength) Ordering[Int].compare(this.length, that.length)
        else if (this.string(this.start + ix) > that.string(that.start + ix)) 1
        else if (this.string(this.start + ix) < that.string(that.start + ix)) -1
        else loop(ix + 1)
      }

      loop(0)
    }

    def startsWith(pattern: StringSlice): Boolean = {
      @tailrec
      def loop(ix: Int): Boolean = {
        if (ix == pattern.length) true
        else if (this.string(this.start + ix) == pattern.string(pattern.start + ix)) loop(ix + 1)
        else false
      }

      if (pattern.length > this.length) false
      else loop(0)
    }
  }

  private class SuffixArrayBuilder(text: String) {
    val length: Int = text.length

    private def sortSingleCharacters(): (Vector[Int], Vector[Int]) = {
      val occurrences: Map[Char, List[Int]] =
        text
          .zipWithIndex
          .groupBy{ case (character, _) => character }
          .mapValues(_.map{ case (_, ix) => ix }.toList)
      val sortedKeys: Vector[Char] = occurrences.keys.toVector.sorted
      val indices: Map[Char, Int] = sortedKeys.zipWithIndex.toMap
      val order: Vector[Int] = sortedKeys.flatMap(occurrences(_))
      val classes: Vector[Int] = text.map(indices(_)).toVector
      (order, classes)
    }

    private def countCumulativeClassSizes(classes: Vector[Int]): Array[Int] = {
      val cumulativeCounts: Array[Int] = Array.fill(length)(0)
      classes.foreach(classId => cumulativeCounts(classId) += 1)
      (1 until length).foreach(ix => cumulativeCounts(ix) += cumulativeCounts(ix - 1))
      cumulativeCounts
    }

    private def sortDoubledShifts(cyclicShiftSize: Int, order: Vector[Int], classes: Vector[Int]): Vector[Int] = {
      val counts: Array[Int] = countCumulativeClassSizes(classes)
      val newOrder: Array[Int] = Array.fill(length)(0)
      for { ix <- (length - 1) to 0 by -1 } {
        val start: Int = (order(ix) - cyclicShiftSize) mod length
        val classId: Int = classes(start)
        counts(classId) -= 1
        newOrder(counts(classId)) = start
      }
      newOrder.toVector
    }

    private def updateClasses(order: Vector[Int], classes: Vector[Int], cyclicShiftSize: Int): Vector[Int] = {
      val labelGenerator: Iterator[Int] = Iterator.from(0)
      val newClasses: Array[Int] = Array.fill(length)(0)
      val label: Int = labelGenerator.next()
      newClasses(order(0)) = label
      val orders: Iterator[Vector[Int]] = order.sliding(2)

      @tailrec
      def loop(currentLabel: Int): Unit = {
        if (orders.isEmpty) ()
        else {
          val Vector(previous, current): Vector[Int] = orders.next()
          val mid: Int = (current + cyclicShiftSize) mod length
          val midPrevious: Int = (previous + cyclicShiftSize) mod length
          val nextLabel: Int =
            if (classes(current) != classes(previous) || classes(mid) != classes(midPrevious)) labelGenerator.next()
            else currentLabel
          newClasses(current) = nextLabel
          loop(nextLabel)
        }
      }
      loop(label)
      newClasses.toVector
    }

    def calcSuffixArray(): Vector[Int] = {
      val (order, classes): (Vector[Int], Vector[Int]) = sortSingleCharacters()

      @tailrec
      def loop(cyclicShiftSize: Int, cyclicShiftOrders: Vector[Int], equivalenceClasses: Vector[Int]): Vector[Int] = {
        if (cyclicShiftSize >= length) cyclicShiftOrders
        else {
          val doubleShiftOrders: Vector[Int] =
            sortDoubledShifts(cyclicShiftSize, cyclicShiftOrders, equivalenceClasses)
          val updatedClasses: Vector[Int] = updateClasses(doubleShiftOrders, equivalenceClasses, cyclicShiftSize)
          loop(2 * cyclicShiftSize, doubleShiftOrders, updatedClasses)
        }
      }

      loop(1, order, classes)
    }
  }

  private def matchPatternWithSuffixArray(suffixArray: Vector[Int],
                                          text: String,
                                          length: Int,
                                          pattern: StringSlice): Set[Int] = {
    @tailrec
    def findMinIndex(minIndex: Int, maxIndex: Int): Int = {
      if (minIndex >= maxIndex) minIndex
      else {
        val middleIndex: Int = (minIndex + maxIndex) / 2
        if (pattern > StringSlice(text, suffixArray(middleIndex), length)) findMinIndex(middleIndex + 1, maxIndex)
        else findMinIndex(minIndex, middleIndex)
      }
    }

    @tailrec
    def findMaxIndex(minIndex: Int, maxIndex: Int): Int = {
      if (minIndex >= maxIndex) maxIndex
      else {
        val middleIndex: Int = (minIndex + maxIndex) / 2
        val textSlice = StringSlice(text, suffixArray(middleIndex), length)
        if (textSlice.startsWith(pattern)) findMaxIndex(middleIndex + 1, maxIndex)
        else if (pattern < textSlice) findMaxIndex(minIndex, middleIndex)
        else findMaxIndex(middleIndex + 1, maxIndex)
      }
    }

    val minIndex: Int = findMinIndex(0, length)
    val maxIndex: Int = findMaxIndex(minIndex, length)
    if (minIndex > maxIndex) Set()
    else (minIndex until maxIndex).map(suffixArray)(breakOut)
  }

  def multiplePatternMatching(text: String, patterns: List[StringSlice]): Set[Int] = {
    val builder = new SuffixArrayBuilder(text)
    val suffixArray: Vector[Int] = builder.calcSuffixArray()

    @tailrec
    def loop(matchedIndices: Set[Int], patterns: List[StringSlice]): Set[Int] = patterns match {
      case Nil => matchedIndices
      case pattern :: rest =>
        val indices: Set[Int] = matchPatternWithSuffixArray(suffixArray, text, builder.length, pattern)
        loop(matchedIndices ++ indices, rest)
    }

    loop(Set.empty[Int], patterns)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next() + StringTerminator
    val _: Int = reader.next().toInt
    val patterns: List[StringSlice] =
      reader.next().split(" ").map(pattern => StringSlice(pattern, 0, pattern.length)).toList
    val result: Set[Int] = multiplePatternMatching(text, patterns)
    println(result.mkString(" "))
  }
}
