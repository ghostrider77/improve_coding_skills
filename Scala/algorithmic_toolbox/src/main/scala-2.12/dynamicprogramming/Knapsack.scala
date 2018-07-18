package dynamicprogramming

object Knapsack {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def solveKnapsackProblem(weights: List[Int], nrWeights: Int, capacity: Int): Int = {
    val knapsack: Array[Array[Int]] = Array.fill(capacity, nrWeights)(-1)

    def solve(currentCapacity: Int, ws: List[(Int, Int)]): Int = ws match {
      case Nil => 0
      case (w, ix) :: wss =>
        if (currentCapacity == 0) 0
        else if (knapsack(currentCapacity-1)(ix) != -1) knapsack(currentCapacity-1)(ix)
        else {
          val optimalCapacity: Int =
            if (currentCapacity < w) solve(currentCapacity, wss)
            else math.max(solve(currentCapacity - w, wss) + w, solve(currentCapacity, wss))
          knapsack(currentCapacity-1)(ix) = optimalCapacity
          optimalCapacity
        }
    }

    solve(capacity, weights.zipWithIndex)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(capacity, nrWeights): List[Int] = convertToIntList(reader.next())
    val weights: List[Int] = convertToIntList(reader.next())
    val result: Int = solveKnapsackProblem(weights, nrWeights, capacity)
    println(result)
  }
}
