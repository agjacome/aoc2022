package dev.agjacome.aoc2022

final class Day04Test extends DayTest(Day04) {

  val input = LazyList(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  val expected = Result(part1 = "2", part2 = "4")

}
