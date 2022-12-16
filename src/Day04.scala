package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Interval

object Day04 extends Day {

  private val RangeLine = """^(\d+)-(\d+),(\d+)-(\d+)$""".r

  def parse(line: String): Option[(Interval, Interval)] = {
    line match {
      case RangeLine(min1, max1, min2, max2) =>
        val left  = Interval(min1.toInt, max1.toInt)
        val right = Interval(min2.toInt, max2.toInt)
        Some((left, right))

      case _ =>
        None
    }
  }

  def run(lines: LazyList[String]): Result = {
    val ranges = lines.flatMap(parse)

    val part1 = ranges.count { case (l, r) => l.contains(r) || r.contains(l) }
    val part2 = ranges.count { case (l, r) => l.overlaps(r) }

    Result(part1.toString, part2.toString)
  }

}
