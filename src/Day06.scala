package dev.agjacome.aoc2022

object Day06 extends Day {

  def findMarkerIndex(markerLength: Int)(signal: String): Int = {
    val slideIndex = signal
      .sliding(markerLength)
      .indexWhere(_.distinct.length == markerLength)

    if (slideIndex >= 0)
      slideIndex + markerLength
    else
      -1
  }

  val findPacketMarker  = findMarkerIndex(4)
  val findMessageMarker = findMarkerIndex(14)

  def run(lines: LazyList[String]): Result = {
    val part1 = lines.map(findPacketMarker).mkString(",")
    val part2 = lines.map(findMessageMarker).mkString(",")

    Result(part1, part2)
  }

}
