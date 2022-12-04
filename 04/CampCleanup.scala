#!/usr/bin/env scala

import scala.io.Source

final case class RangePair(r1: Range, r2: Range) {

  def fullyContains: Boolean =
    r1.containsSlice(r2) || r2.containsSlice(r1)

  def overlaps: Boolean =
    r1.intersect(r2).nonEmpty

}

object Day4 extends App {

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

  val source = args.headOption.fold(Source.stdin)(Source.fromFile)

  val ranges = source.getLines.flatMap(parseRangeLine).to(LazyList)

  val part1 = ranges.count(_.fullyContains)
  val part2 = ranges.count(_.overlaps)

  println(s"Part 1: ${part1}")
  println(s"Part 2: ${part2}")

}
