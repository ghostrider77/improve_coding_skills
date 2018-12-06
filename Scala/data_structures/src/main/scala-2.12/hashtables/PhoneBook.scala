package hashtables

object PhoneBook {
  import scala.annotation.tailrec

  sealed trait Query {
    val number: Int
  }
  case class Addition(number: Int, name: String) extends Query
  case class Deletion(number: Int) extends Query
  case class Find(number: Int) extends Query

  class PhoneBook(maxSize: Int) {
    private val phoneBook: Array[Option[String]] = Array.fill(maxSize)(None)

    def add(number: Int, name: String): Unit = phoneBook(number) = Some(name)

    def delete(number: Int): Unit = phoneBook(number) = None

    def find(number: Int): Option[String] = phoneBook(number)
  }

  private val MaxSize: Int = 10000000

  private def convertToList(line: String): List[String] = line.split(" ").toList

  private[hashtables] def readQueries(reader: Iterator[String], nrQueries: Int): List[Query] = {
    (for { _ <- 0 until nrQueries } yield {
      convertToList(reader.next()) match {
        case List("add", number, name) => Addition(number.toInt, name)
        case List("del", number) => Deletion(number.toInt)
        case List("find", number) => Find(number.toInt)
        case _ => throw new Exception("Unknown query type")
      }
    }).toList
  }

  def processQueries(queries: List[Query]): List[String] = {
    val phoneBook = new PhoneBook(MaxSize)
    @tailrec
    def loop(acc: List[String], qs: List[Query]): List[String] = qs match {
      case Nil => acc.reverse
      case Addition(number, name) :: qss =>
          phoneBook.add(number, name)
          loop(acc, qss)
      case Deletion(number) :: qss =>
          phoneBook.delete(number)
          loop(acc, qss)
      case Find(number) :: qss => phoneBook.find(number) match {
        case None => loop("not found" :: acc, qss)
        case Some(name) => loop(name :: acc, qss)
      }
    }
    loop(Nil, queries)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrQueries: Int = reader.next().toInt
    val queries: List[Query] = readQueries(reader, nrQueries)
    val results: List[String] = processQueries(queries)
    results.foreach(println)
  }
}
