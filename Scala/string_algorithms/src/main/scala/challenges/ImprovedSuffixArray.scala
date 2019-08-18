package challenges

object ImprovedSuffixArray {
  import scala.annotation.tailrec
  import utils.Utils.RichNumber._

  private def sortSingleCharacters(text: String): (Vector[Int], Vector[Int]) = {
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

  private def countCumulativeClassSizes(classes: Vector[Int], length: Int): Array[Int] = {
    val cumulativeCounts: Array[Int] = Array.fill(length)(0)
    classes.foreach(classId => cumulativeCounts(classId) += 1)
    (1 until length).foreach(ix => cumulativeCounts(ix) += cumulativeCounts(ix - 1))
    cumulativeCounts
  }

  private def sortDoubledShifts(cyclicShiftSize: Int,
                                order: Vector[Int],
                                classes: Vector[Int],
                                length: Int): Vector[Int] = {
    val counts: Array[Int] = countCumulativeClassSizes(classes, length)
    val newOrder: Array[Int] = Array.fill(length)(0)
    for { ix <- (length - 1) to 0 by -1 } {
      val start: Int = (order(ix) - cyclicShiftSize) mod length
      val classId: Int = classes(start)
      counts(classId) -= 1
      newOrder(counts(classId)) = start
    }
    newOrder.toVector
  }

  private def updateClasses(order: Vector[Int],
                            classes: Vector[Int],
                            cyclicShiftSize: Int,
                            length: Int): Vector[Int] = {
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

  def calcSuffixArray(text: String): List[Int] = {
    val (order, classes): (Vector[Int], Vector[Int]) = sortSingleCharacters(text)
    val length: Int = text.length

    @tailrec
    def loop(cyclicShiftSize: Int, cyclicShiftOrders: Vector[Int], equivalenceClasses: Vector[Int]): List[Int] = {
      if (cyclicShiftSize >= length) cyclicShiftOrders.toList
      else {
        val doubleShiftOrders: Vector[Int] =
          sortDoubledShifts(cyclicShiftSize, cyclicShiftOrders, equivalenceClasses, length)
        val updatedClasses: Vector[Int] = updateClasses(doubleShiftOrders, equivalenceClasses, cyclicShiftSize, length)
        loop(2 * cyclicShiftSize, doubleShiftOrders, updatedClasses)
      }
    }

    loop(1, order, classes)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val result: List[Int] = calcSuffixArray(text)
    println(result.mkString(" "))
  }
}
