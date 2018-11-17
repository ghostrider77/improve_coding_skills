package heaps_and_sets

import org.scalatest.{FreeSpec, Matchers}

class HeapsAndSetsSuite extends FreeSpec with Matchers {

  "Heapify" - {
    import Heapify.{IndexPair, heapify, swapElems}

    val heapifyConst: Int = 4

    def isHeap(array: Array[Int]): Boolean = {
      val length: Int = array.length
      def minHeapProperty(ix: Int): Boolean = {
        val leftChildRelation: Boolean = if (2*ix + 1 < length) array(ix) <= array(2 * ix + 1) else true
        val rightChildRelation: Boolean = if (2*ix + 2 < length) array(ix) <= array(2 * ix + 2) else true
        leftChildRelation && rightChildRelation
      }
      if (length <= 1) true
      else {
        val maxParentIx: Int = length / 2 - 1
        (0 to maxParentIx).forall(minHeapProperty)
      }
    }

    "should not do any swaps when the array is already sorted" in {
      val array: Array[Int] = Array(1, 2, 3, 4, 5)
      heapify(array, array.length) shouldBe empty
    }

    "should create a heap from an array in linear time" - {
      "test case 1" in {
        val array: Array[Int] = Array(5, 4, 3, 2, 1)
        val n: Int = array.length
        val swaps: List[IndexPair] = heapify(array.clone(), n)
        swaps.length should be <= heapifyConst * n
        swaps.foreach{ case (i, j) => swapElems(array, i, j) }
        isHeap(array) shouldBe true
      }

      "test case 2" in {
        val array: Array[Int] = Array(10, 9, 4, 5, 6, 1, 2, 3)
        val n: Int = array.length
        val swaps: List[IndexPair] = heapify(array.clone(), n)
        swaps.length should be <= heapifyConst * n
        swaps.foreach{ case (i, j) => swapElems(array, i, j) }
        isHeap(array) shouldBe true
      }
    }
  }

  "ParallelProcessing" - {
    import ParallelProcessing.{JobSchedule, processJobs}

    "should calculate which thread starts processing the jobs" - {
      "test case 1" in {
        val nrThreads: Int = 2
        val processingTimes: List[Int] = List(1, 2, 3, 4, 5)
        val nrJobs: Int = processingTimes.length
        processJobs(processingTimes, nrJobs, nrThreads) shouldEqual
          Array(JobSchedule(0, 0), JobSchedule(1, 0), JobSchedule(0, 1), JobSchedule(1, 2), JobSchedule(0, 4))
      }

      "test case 2" in {
        val nrThreads: Int = 3
        val processingTimes: List[Int] = List(2, 4)
        processJobs(processingTimes, processingTimes.length, nrThreads) shouldEqual
          Array(JobSchedule(0, 0), JobSchedule(1, 0))
      }

      "test case 3" in {
        val nrThreads: Int = 1
        val time: Int = 1e9.toInt
        val processingTimes: List[Int] = List(time, time, time, time)
        processJobs(processingTimes, processingTimes.length, nrThreads) shouldEqual
          Array(JobSchedule(0, 0), JobSchedule(0, 1e9.toLong), JobSchedule(0, 2e9.toLong), JobSchedule(0, 3e9.toLong))
      }

      "test case 4" in {
        val nrThreads: Int = 3
        val nrJobs: Int = 12
        val processingTimes: List[Int] = List.fill(nrJobs)(1)
        processJobs(processingTimes, nrJobs, nrThreads) shouldEqual
          Array(
            JobSchedule(0, 0),
            JobSchedule(1, 0),
            JobSchedule(2, 0),
            JobSchedule(0, 1),
            JobSchedule(1, 1),
            JobSchedule(2, 1),
            JobSchedule(0, 2),
            JobSchedule(1, 2),
            JobSchedule(2, 2),
            JobSchedule(0, 3),
            JobSchedule(1, 3),
            JobSchedule(2, 3)
          )
      }
    }
  }

  "MergingTables" - {
    import MergingTables.{processMergeRequests, TableOperation}

    "should calculate the largest table after each database merge operation" - {
      "test case 1" in {
        val nrTables: Int = 5
        val tableRows: List[Int] = List.fill(nrTables)(1)
        val operations: List[TableOperation] =
          List(
            TableOperation(destination = 2, source = 4),
            TableOperation(destination = 1, source = 3),
            TableOperation(destination = 0, source = 3),
            TableOperation(destination = 4, source = 3),
            TableOperation(destination = 4, source = 2)
          )
        processMergeRequests(tableRows, nrTables, operations) shouldEqual List(2, 2, 3, 5, 5)
      }

      "test case 2" in {
        val tableRows: List[Int] = List(10, 0, 5, 0, 3, 3)
        val operations: List[TableOperation] =
          List(
            TableOperation(destination = 5, source = 5),
            TableOperation(destination = 5, source = 4),
            TableOperation(destination = 4, source = 3),
            TableOperation(destination = 3, source = 2)
          )
        processMergeRequests(tableRows, tableRows.length, operations) shouldEqual List(10, 10, 10, 11)
      }
    }
  }
}
