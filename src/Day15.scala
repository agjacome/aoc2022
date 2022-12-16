package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.Point.DistanceMetric._

object Day15 extends Day {


  final case class Sensor(position: Point, distanceToBeacon: Int) {

    def rowCoverage(row: Int): Option[Interval] = {
      val rowDifference = (row - position.row).abs

      Option.when(rowDifference <= distanceToBeacon) {
        val colOffset = (distanceToBeacon - rowDifference).abs
        Interval(position.col - colOffset, position.col + colOffset)
      }
    }

  }

  object Sensor {

    private val SensorLine =
      """^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$""".r

    def parse(line: String): Option[Sensor] =
      line match {
        case SensorLine(sx, sy, bx, by) =>
          val sensor = Point(col = sx.toInt, row = sy.toInt)
          val beacon = Point(col = bx.toInt, row = by.toInt)

          val distance = sensor.distanceTo(beacon, metric = Manhattan)

          Some(Sensor(sensor, distance))

        case _ => None
      }

  }

  final case class SensorSet(sensors: Set[Sensor]) {

    private val maxRow = sensors.map(_.position.row).max

    def rowCoverage(row: Int): Set[Interval] = {
      val intervals = sensors.flatMap(_.rowCoverage(row))
      IntervalSet(intervals).disjointUnion
    }

    def findDistressBeacon(rowCount: Int): Option[Point] =
      (0 to Math.min(maxRow, rowCount))
        .map(row => (row, this.rowCoverage(row)))
        .find { case (_, rowCoverages) => rowCoverages.size > 1 }
        .map { case (row, rowCoverages) => Point(row, rowCoverages.head.high + 1) }

  }

  object SensorSet {

    def parse(lines: LazyList[String]): SensorSet = {
      val sensors = lines.flatMap(Sensor.parse).to(Set)
      SensorSet(sensors)
    }

  }

  final case class Interval(low: Int, high: Int) {

    def overlaps(that: Interval): Boolean =
      Math.max(this.low, that.low) <= Math.min(this.high, that.high)

    def borders(that: Interval): Boolean =
      Math.max(this.low, that.low) == Math.min(this.high, that.high) + 1

    def union(that: Interval): Option[Interval] =
      Option.when(this.overlaps(that) || this.borders(that)) {
        val low  = Math.min(this.low, that.low)
        val high = Math.max(this.high, that.high)
        Interval(low, high)
      }

    def size: Int =
      (high - low).abs

  }

  object Interval {
    implicit val ord: Ordering[Interval] = Ordering.by(_.low)
  }

  final case class IntervalSet(intervals: Set[Interval]) {

    lazy val disjointUnion: Set[Interval] = {
      @scala.annotation.tailrec
      def loop(prev: Interval, intervals: List[Interval], acc: Set[Interval]): Set[Interval] =
        intervals match {
          case Nil => acc + prev
          case curr :: next =>
            prev.union(curr) match {
              case Some(union) => loop(union, next, acc)
              case None        => loop(curr, next, acc + prev)
            }
        }

      intervals.to(List).sorted match {
        case head :: tail => loop(head, tail, Set.empty)
        case Nil          => Set.empty
      }
    }

  }

  def run(lines: LazyList[String]): Result = {
    val sensors = SensorSet.parse(lines)

    val part1 = {
      val row      = 10 // 2_000_000
      val coverage = sensors.rowCoverage(row)

      coverage.headOption.fold(-1)(_.size)
    }

    val part2 = {
      val limit  = 4_000_000
      val beacon = sensors.findDistressBeacon(limit)

      beacon.fold(-1L)(p => p.col.toLong * limit + p.row)
    }

    Result(part1.toString, part2.toString)
  }

}
