package dynamicprogramming

object ArithmeticExpression {
  private val Operations: Map[String, (Long, Long) => Long] =
    Map("+" -> ((a, b) => a + b), "-" -> ((a, b) => a - b), "*" -> ((a, b) => a * b))

  private[dynamicprogramming] def readInputData(line: String): (List[Long], Vector[String]) = {
    val digits: List[Long] = line.sliding(1, 2).map(_.toLong).toList
    val operations: Vector[String] = line.drop(1).sliding(1, 2).toVector
    (digits, operations)
  }

  private def calcMinMax(ix: Int,
                         jy: Int,
                         operations: Vector[String],
                         minimumOfSubexpressions: Array[Array[Long]],
                         maximumOfSubexpressions: Array[Array[Long]]): (Long, Long) = {
    def calcSubExpressionValuesInRange(acc: (Long, Long), k: Int): (Long, Long) = {
      val (subExpressionMin, subExpressionMax): (Long, Long) = acc
      val op: (Long, Long) => Long = Operations(operations(k))
      val subexpressionsSplitAtK: List[Long] =
        List(
          op(maximumOfSubexpressions(ix)(k), maximumOfSubexpressions(k + 1)(jy)),
          op(maximumOfSubexpressions(ix)(k), minimumOfSubexpressions(k + 1)(jy)),
          op(minimumOfSubexpressions(ix)(k), maximumOfSubexpressions(k + 1)(jy)),
          op(minimumOfSubexpressions(ix)(k), minimumOfSubexpressions(k + 1)(jy))
          )
      (math.min(subExpressionMin, subexpressionsSplitAtK.min), math.max(subExpressionMax, subexpressionsSplitAtK.max))
    }

    (ix until jy).foldLeft((Long.MaxValue, Long.MinValue))(calcSubExpressionValuesInRange)
  }

  def maximizeExpression(digits: List[Long], operations: Vector[String]): Long = {
    val n: Int = digits.length
    val minimumOfSubexpressions: Array[Array[Long]] = Array.fill(n, n)(0L)
    val maximumOfSubexpressions: Array[Array[Long]] = Array.fill(n, n)(0L)
    for { (digit, ix) <- digits.zipWithIndex } {
      minimumOfSubexpressions(ix)(ix) = digit
      maximumOfSubexpressions(ix)(ix) = digit
    }
    for {
      s <- 1 until n
      ix <- 0 until n - s
    } {
      val jy: Int = ix + s
      val (subMin, subMax): (Long, Long) =
        calcMinMax(ix, jy, operations, minimumOfSubexpressions, maximumOfSubexpressions)
      minimumOfSubexpressions(ix)(jy) = subMin
      maximumOfSubexpressions(ix)(jy) = subMax
    }
    maximumOfSubexpressions(0)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (digits, operations): (List[Long], Vector[String]) = readInputData(reader.next())
    val result: Long = maximizeExpression(digits, operations)
    println(result)
  }
}
