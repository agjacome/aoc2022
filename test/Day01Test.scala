package dev.agjacome.aoc2022

final class Day01Test extends DayTest(Day01) {

  val input = LazyList(
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  )

  val expected = Result(part1 = "24000", part2 = "45000")

}
