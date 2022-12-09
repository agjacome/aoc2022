package dev.agjacome.aoc2022

object Day06 extends Day {

  val packetMarkerLength  = 4
  val messageMarkerLength = 14

  def findMarkerIndex(signal: String, markerLength: Int): Option[Int] = {
    val slideIndex = signal
      .sliding(markerLength)
      .indexWhere(_.distinct.length == markerLength)

    Option(slideIndex).filter(_ >= 0).map(_ + markerLength)
  }

  def run(lines: Iterator[String]): Result = {
    val markers = lines
      .flatMap { signal =>
        for {
          startOfPacket  <- findMarkerIndex(signal, packetMarkerLength)
          startOfMessage <- findMarkerIndex(signal, messageMarkerLength)
        } yield (startOfPacket, startOfMessage)
      }
      .to(LazyList)

    Result(
      part1 = markers.map(_._1).mkString(","),
      part2 = markers.map(_._2).mkString(",")
    )
  }

}
