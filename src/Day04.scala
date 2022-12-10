package dev.agjacome.aoc2022

final case class RangePair(r1: Range, r2: Range) {

  def fullyContains: Boolean =
    r1.containsSlice(r2) || r2.containsSlice(r1)

  def overlaps: Boolean =
    r1.intersect(r2).nonEmpty

}

object Day04 extends Day {

  val LineRegex = """^(\d+)-(\d+),(\d+)-(\d+)$""".r

  def parseRangeLine(line: String): Option[RangePair] = {
    line match {
      case LineRegex(min1, max1, min2, max2) =>
        val left  = (min1.toInt to max1.toInt)
        val right = (min2.toInt to max2.toInt)
        Some(RangePair(left, right))

      case _ =>
        None
    }
  }

  def run(lines: LazyList[String]): Result = {
    val ranges = lines.flatMap(parseRangeLine)

    val part1 = ranges.count(_.fullyContains)
    val part2 = ranges.count(_.overlaps)

    Result(part1.toString, part2.toString)
  }

}
