package heaps_and_sets

object ParallelProcessing {
  import scala.collection.mutable.{PriorityQueue => Heap}

  final case class ProcessedJob(threadId: Int, jobId: Int, processTime: Int, finishTime: Long)

  object ProcessedJob {
    private def lt(p: ProcessedJob, q: ProcessedJob): Boolean =
      p.finishTime < q.finishTime || (p.finishTime == q.finishTime && p.threadId < q.threadId)

    implicit def priority: Ordering[ProcessedJob] = Ordering.fromLessThan(lt)
  }

  final case class JobSchedule(threadId: Int, processStartTime: Long)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def createInitialHeap(initialJobs: List[Int]): Heap[ProcessedJob] = {
    val heap: Heap[ProcessedJob] = Heap.empty[ProcessedJob](Ordering[ProcessedJob].reverse)
    initialJobs
      .zipWithIndex
      .foreach { case (processTime, id) => heap.enqueue(ProcessedJob(id, id, processTime, processTime.toLong)) }
    heap
  }

  private def removeRemainingElementsFromHeap(heap: Heap[ProcessedJob],
                                              jobsProcessedByThreads: Array[JobSchedule]): Unit = {
    while (heap.nonEmpty) {
      val ProcessedJob(threadId, jobId, processTime, finishTime) = heap.dequeue()
      jobsProcessedByThreads(jobId) = JobSchedule(threadId, finishTime - processTime)
    }
  }

  def processJobs(jobProcessingTimes: List[Int], nrJobs: Int, nrThreads: Int): Array[JobSchedule] = {
    if (nrJobs <= nrThreads) (0 until nrJobs).map(id => JobSchedule(id, 0)).toArray
    else {
      val jobsProcessedByThreads: Array[JobSchedule] = new Array[JobSchedule](nrJobs)
      val (initialJobs, remainingJobs): (List[Int], List[Int]) = jobProcessingTimes.splitAt(nrThreads)
      val heap: Heap[ProcessedJob] = createInitialHeap(initialJobs)
      for { (processTime, jobId) <- remainingJobs.zip(nrThreads until nrJobs) } {
        val ProcessedJob(freeThreadId, finishedJobId, finishedProcessTime, finishedEndTime) = heap.dequeue()
        jobsProcessedByThreads(finishedJobId) = JobSchedule(freeThreadId, finishedEndTime - finishedProcessTime)
        heap.enqueue(ProcessedJob(freeThreadId, jobId, processTime, finishedEndTime + processTime))
      }
      removeRemainingElementsFromHeap(heap, jobsProcessedByThreads)
      jobsProcessedByThreads
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrThreads, nrJobs): List[Int] = convertToIntList(reader.next())
    val jobProcessingTimes: List[Int] = convertToIntList(reader.next())
    val result: Array[JobSchedule] = processJobs(jobProcessingTimes, nrJobs, nrThreads)
    result.foreach{ case JobSchedule(thread, start) => println(s"$thread $start") }
  }
}
