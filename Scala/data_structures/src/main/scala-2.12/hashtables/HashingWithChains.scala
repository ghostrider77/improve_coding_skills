package hashtables

object HashingWithChains {
  import scala.annotation.tailrec

  sealed trait Query
  case class Addition(string: String) extends Query
  case class Deletion(string: String) extends Query
  case class Find(string: String) extends Query
  case class Check(k: Int) extends Query

  case class Parameters(prime: Long, x: Int)

  private val params = Parameters(prime = 1000000007L, x = 263)

  class HashTable(prime: Long, x: Int, nrBuckets: Int) {
    type Chain = List[String]
    val table: Array[Chain] = Array.fill(nrBuckets)(Nil)

    private def polynomialHashing(string: String): Int =
      (string.foldRight(0L){ case (char, acc) => (acc * this.x + char.toInt) % this.prime } % this.nrBuckets).toInt

    def add(string: String): Unit = {
      val hashValue: Int = polynomialHashing(string)
      val chain: Chain = table(hashValue)
      if (!chain.contains(string)) table(hashValue) = string :: chain
    }

    def delete(string: String): Unit = {
      val hashValue: Int = polynomialHashing(string)
      val chain: Chain = table(hashValue)
      table(hashValue) = chain.filterNot(_ == string)
    }

    def find(string: String): String = {
      val hashValue: Int = polynomialHashing(string)
      val chain: Chain = table(hashValue)
      if (chain.contains(string)) "yes" else "no"
    }

    def check(k: Int): String = table(k).mkString(" ")
  }

  private def convertToList(line: String): List[String] = line.split(" ").toList

  private[hashtables] def readQueries(reader: Iterator[String], nrQueries: Int): List[Query] = {
    (for { _ <- 0 until nrQueries } yield {
      convertToList(reader.next()) match {
        case List("add", string) => Addition(string)
        case List("del", string) => Deletion(string)
        case List("find", string) => Find(string)
        case List("check", bucketId) => Check(bucketId.toInt)
        case _ => throw new Exception("Unknown query type")
      }
    }).toList
  }

  def processQueries(queries: List[Query], nrBuckets: Int): List[String] = {
    val hashTable = new HashTable(prime = params.prime, x = params.x, nrBuckets = nrBuckets)
    @tailrec
    def loop(acc: List[String], qs: List[Query]): List[String] = qs match {
      case Nil => acc.reverse
      case Addition(string) :: qss =>
        hashTable.add(string)
        loop(acc, qss)
      case Deletion(string) :: qss =>
        hashTable.delete(string)
        loop(acc, qss)
      case Find(string) :: qss => loop(hashTable.find(string) :: acc, qss)
      case Check(bucket) :: qss => loop(hashTable.check(bucket) :: acc, qss)
    }
    loop(Nil, queries)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrBuckets: Int = reader.next().toInt
    val nrQueries: Int = reader.next().toInt
    val queries: List[Query] = readQueries(reader, nrQueries)
    val results: List[String] = processQueries(queries, nrBuckets)
    results.foreach(println)
  }
}
