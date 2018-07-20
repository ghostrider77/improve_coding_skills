package dynamicprogramming

object LongestCommonSubsequence {
  final case class Sequence(seq: Vector[Int], length: Int)

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def readInputData(reader: Iterator[String], k: Int = 3): List[Sequence] =
    reader.take(2 * k).sliding(2).map{ case List(n, seq) => Sequence(convertToIntVector(seq), n.toInt)}.toList

  def calcLongestCommonSubsequence(sequences: List[Sequence]): Int = {
    val (List(s1, s2, s3), List(n1, n2, n3)): (List[Vector[Int]], List[Int]) = sequences.unzip(Sequence.unapply(_).get)
    val longestPath: Array[Array[Array[Int]]] = Array.fill(n1 + 1, n2 + 1, n3 + 1)(0)
    for {
      i <- 0 until n1
      j <- 0 until n2
      k <- 0 until n3
    } {
      longestPath(i + 1)(j + 1)(k + 1) =
        if (s1(i) == s2(j) && s1(i) == s3(k)) longestPath(i)(j)(k) + 1
        else List(longestPath(i)(j + 1)(k + 1), longestPath(i + 1)(j)(k + 1), longestPath(i + 1)(j + 1)(k)).max
    }
    longestPath(n1)(n2)(n3)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val sequences: List[Sequence] = readInputData(reader)
    val result: Int = calcLongestCommonSubsequence(sequences)
    println(result)
  }
}
