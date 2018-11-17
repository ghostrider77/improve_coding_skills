package heaps_and_sets

object MergingTables {
  import scala.annotation.tailrec

  final case class TableOperation(destination: Int, source: Int)

  class UnionFind(nrTables: Int, initialTableRows: Seq[Int]) {
    private val tableRows: Array[Int] = initialTableRows.toArray
    private val parents: Array[Int] = (0 until nrTables).toArray
    private val ranks: Array[Int] = Array.fill(nrTables)(0)

    private def changeParentsToRoot(indicesOnPath: List[Int], root: Int): Unit =
      indicesOnPath.foreach(ix => parents(ix) = root)

    private def find(childIndex: Int): Int = {
      @tailrec
      def loop(id: Int, parentId: Int, indicesTowardsRoot: List[Int]): (Int, List[Int]) = {
        if (id == parentId) (id, indicesTowardsRoot)
        else loop(parentId, parents(parentId), id :: indicesTowardsRoot)
      }
      val (root, indicesOnPath): (Int, List[Int]) = loop(childIndex, parents(childIndex), Nil)
      changeParentsToRoot(indicesOnPath, root)
      root
    }

    def union(source: Int, destination: Int, largestTableSize: Int): Int = {
      val sourceId: Int = find(source)
      val destId: Int = find(destination)
      if (sourceId == destId) largestTableSize
      else if (ranks(sourceId) > ranks(destId)) {
        parents(destId) = sourceId
        tableRows(sourceId) += tableRows(destId)
        tableRows(destId) = 0
        math.max(largestTableSize, tableRows(sourceId))
      } else {
        parents(sourceId) = destId
        tableRows(destId) += tableRows(sourceId)
        tableRows(sourceId) = 0
        if (ranks(sourceId) == ranks(destId)) ranks(destId) += 1
        math.max(largestTableSize, tableRows(destId))
      }
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readTableOperations(reader: Iterator[String], nrOperations: Int): List[TableOperation] =
    (for { _ <- 0 until nrOperations } yield {
      val List(d, s): List[Int] = convertToIntList(reader.next())
      TableOperation(destination = d - 1, source = s - 1)
    }).toList

  def processMergeRequests(tableRows: List[Int], nrTables: Int, operations: List[TableOperation]): List[Int] = {
    val tables = new UnionFind(nrTables, tableRows)
    @tailrec
    def loop(ops: List[TableOperation], largestTableSize: Int, acc: List[Int]): List[Int] = ops match {
      case Nil => acc.reverse
      case TableOperation(destination, source) :: rest =>
        val updatedSize: Int = tables.union(source, destination, largestTableSize)
        loop(rest, updatedSize, updatedSize :: acc)
      }

    val initialMaxSize: Int = tableRows.max
    loop(operations, initialMaxSize, Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrTables, nrOperations): List[Int] = convertToIntList(reader.next())
    val tableRows: List[Int] = convertToIntList(reader.next())
    val operations: List[TableOperation] = readTableOperations(reader, nrOperations)
    val result: List[Int] = processMergeRequests(tableRows, nrTables, operations)
    result.foreach(println)
  }
}
