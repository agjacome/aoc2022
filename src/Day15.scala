package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Interval
import dev.agjacome.aoc2022.util.Point

object Day15 extends Day {

  final case class Sensor(position: Point, closestBeacon: Point) {

    import Point.DistanceMetric._

    val distanceToBeacon: Int =
      position.distanceTo(closestBeacon, metric = Manhattan)

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
          Some(Sensor(sensor, beacon))

        case _ => None
      }

  }

  final case class SensorSet(sensors: Set[Sensor]) {

    private val maxRow = sensors.map(_.position.row).max

    def rowCoverage(row: Int): Set[Interval] = {
      val intervals = sensors.flatMap(_.rowCoverage(row min maxRow))
      Interval.disjointUnion(intervals)
    }

    def findDistressBeacon(rowCount: Int): Option[Point] =
      Iterator
        .range(0, rowCount min maxRow)
        .map(row => (row, rowCoverage(row)))
        .collectFirst {
          case (row, coverage) if coverage.size > 1 => Point(row, coverage.head.high + 1)
        }

  }

  object SensorSet {

    def parse(lines: LazyList[String]): SensorSet = {
      val sensors = lines.flatMap(Sensor.parse).to(Set)
      SensorSet(sensors)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val sensors = SensorSet.parse(lines)

    val part1 = {
      val row      = 2_000_000
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
