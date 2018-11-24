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

  "HashingWithChains" - {
    import HashingWithChains.{processQueries, readQueries, Query}

    "should read and write strings into a hash table" - {
      "test case 1" in {
        val nrBuckets: Int = 5
        val nrQueries: Int = 12
        val rawLines: Iterator[String] =
          List(
            "add world",
            "add HellO",
            "check 4",
            "find World",
            "find world",
            "del world",
            "check 4",
            "del HellO",
            "add luck",
            "add GooD",
            "check 2",
            "del good"
          ).toIterator
        val queries: List[Query] = readQueries(rawLines, nrQueries)
        processQueries(queries, nrBuckets) shouldEqual List("HellO world", "no", "yes", "HellO", "GooD luck")
      }

      "test case 2" in {
        val nrBuckets: Int = 4
        val nrQueries: Int = 8
        val rawLines: Iterator[String] =
          List(
            "add test",
            "add test",
            "find test",
            "del test",
            "find test",
            "find Test",
            "add Test",
            "find Test"
          ).toIterator
        val queries: List[Query] = readQueries(rawLines, nrQueries)
        processQueries(queries, nrBuckets) shouldEqual List("yes", "no", "no", "yes")
      }

      "test case 3" in {
        val nrBuckets: Int = 3
        val nrQueries: Int = 12
        val rawLines: Iterator[String] =
          List(
            "check 0",
            "find help",
            "add help",
            "add del",
            "add add",
            "find add",
            "find del",
            "del del",
            "find del",
            "check 0",
            "check 1",
            "check 2"
          ).toIterator
        val queries: List[Query] = readQueries(rawLines, nrQueries)
        processQueries(queries, nrBuckets) shouldEqual List("", "no", "yes", "yes", "no", "", "add help", "")
      }
    }
  }

  "RabinKarp" - {
    import RabinKarp.{findPatternInText, Prime}

    "should find the indices of a text where pattern appears as substring" - {
      "test case 1" in {
        val pattern: String = "aba"
        val text: String = "abacaba"
        findPatternInText(text, pattern, Prime) shouldEqual List(0, 4)
      }

      "test case 2" in {
        val pattern: String = "Test"
        val text: String = "testTesttesT"
        findPatternInText(text, pattern, Prime) shouldEqual List(4)
      }

      "test case 3" in {
        val pattern: String = "aaaaa"
        val text: String = "baaaaaaa"
        findPatternInText(text, pattern, Prime) shouldEqual List(1, 2, 3)
      }
    }
  }
}
