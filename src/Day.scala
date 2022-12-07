package dev.agjacome.aoc2022

final case class Result(part1: String, part2: String)

trait Day {
  def run(lines: Iterator[String]): Result
}

object Day {

  val all: List[Day] = List(
    Day01,
    Day02,
    Day03,
    Day04,
    Day05,
    Day06,
    Day07
  )

  val get: Int => Option[Day] =
    all.zipWithIndex.map { case (day, idx) => (idx + 1, day) }.toMap.get

}

