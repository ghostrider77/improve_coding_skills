package hashtables

import org.scalatest.{FreeSpec, Matchers}

class HashTablesSuite extends FreeSpec with Matchers {

  "PhoneBook" - {
    import PhoneBook.{processQueries, readQueries, Query}

    "should read and write a phone book according to the instructions" - {
      "test case 1" in {
        val nrQueries: Int = 12
        val rawLines: Iterator[String] =
          List(
            "add 911 police",
            "add 76213 Mom",
            "add 17239 Bob",
            "find 76213",
            "find 910",
            "find 911",
            "del 910",
            "del 911",
            "find 911",
            "find 76213",
            "add 76213 daddy",
            "find 76213"
          ).toIterator
        val queries: List[Query] = readQueries(rawLines, nrQueries)
        processQueries(queries) shouldEqual List("Mom", "not found", "police", "not found", "Mom", "daddy")
      }

      "test case 2" in {
        val nrQueries: Int = 8
        val rawLines: Iterator[String] =
          List(
            "find 3839442",
            "add 123456 me",
            "add 0 granny",
            "find 0",
            "find 123456",
            "del 0",
            "del 0",
            "find 0"
          ).toIterator
        val queries: List[Query] = readQueries(rawLines, nrQueries)
        processQueries(queries) shouldEqual List("not found", "granny", "me", "not found")
      }
    }
  }
}
