package introduction

object NetworkPacketProcessing {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}

  final case class Packet(id: Int, arrivalTime: Int, processingTime: Int)
  final case class BufferedPacket(packet: Packet, finishTime: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def removeFinishedPacketsFromBuffer(currentTime: Int,
                                              buffer: MutableQueue[BufferedPacket]): List[BufferedPacket] = {
    @tailrec
    def remove(removedPackets: List[BufferedPacket]): List[BufferedPacket] = {
      if (buffer.isEmpty) removedPackets
      else if (buffer.front.finishTime <= currentTime) {
        val firstPacket: BufferedPacket = buffer.dequeue()
        remove(firstPacket :: removedPackets)
      } else removedPackets
    }
    remove(Nil)
  }

  private def addStartTimeToFinishedPackets(processStartTime: Array[Int],
                                            finishedPackets: List[BufferedPacket]): Unit = {
    for { BufferedPacket(Packet(id, _, processTime), finishTime) <- finishedPackets }
      processStartTime(id) = finishTime - processTime
  }

  def processPackets(networkPackets: List[Packet], maxBufferSize: Int, nrPackets: Int): Array[Int] = {
    val processStartTime: Array[Int] = Array.fill(nrPackets)(0)
    val buffer: MutableQueue[BufferedPacket] = MutableQueue()

    def insertPacketToBuffer(packet: Packet): Unit = {
      val finishedPackets: List[BufferedPacket] = removeFinishedPacketsFromBuffer(packet.arrivalTime, buffer)
      addStartTimeToFinishedPackets(processStartTime, finishedPackets)
      if (buffer.isEmpty) buffer.enqueue(BufferedPacket(packet, packet.arrivalTime + packet.processingTime))
      else if (buffer.size >= maxBufferSize) processStartTime(packet.id) = -1
      else {
        val lastPacket: BufferedPacket = buffer.last
        buffer.enqueue(BufferedPacket(packet, lastPacket.finishTime + packet.processingTime))
      }
    }

    networkPackets.foreach(insertPacketToBuffer)
    addStartTimeToFinishedPackets(processStartTime, buffer.toList)
    processStartTime
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(maxBufferSize, nrPackets): List[Int] = convertToIntList(reader.next())
    val networkPackets: List[Packet] = (for { id <- 0 until nrPackets } yield {
      val List(arrival, process): List[Int] = convertToIntList(reader.next())
      Packet(id = id, arrivalTime = arrival, processingTime = process)
    }).toList
    val processStartTimeOfPackets: Array[Int] = processPackets(networkPackets, maxBufferSize, nrPackets)
    processStartTimeOfPackets.foreach(println)
  }
}
