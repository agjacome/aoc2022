package dev.agjacome.aoc2022

final case class Result(part1: String, part2: String)

trait Day {
  def run(lines: LazyList[String]): Result
}

object Day {

  val all: Map[Int, Day] = Map(
    1  -> Day01,
    2  -> Day02,
    3  -> Day03,
    4  -> Day04,
    5  -> Day05,
    6  -> Day06,
    7  -> Day07,
    8  -> Day08,
    9  -> Day09,
    10 -> Day10,
    11 -> Day11,
    12 -> Day12
  )

  val get: Int => Option[Day] = all.get

}
