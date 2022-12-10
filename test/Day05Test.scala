package dev.agjacome.aoc2022

final class Day05Test extends DayTest(Day05) {

  val input = LazyList(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  )

  val expected = Result(part1 = "CMZ", part2 = "MCD")

}
