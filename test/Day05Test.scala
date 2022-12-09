package dev.agjacome.aoc2022

final class Day05Test extends DayTest(
      day = Day05,
      testInputLines = List(
        "    [D]    ",
        "[N] [C]    ",
        "[Z] [M] [P]",
        " 1   2   3 ",
        "",
        "move 1 from 2 to 1",
        "move 3 from 1 to 3",
        "move 2 from 2 to 1",
        "move 1 from 1 to 2"
      ),
      expectedResult = Result(part1 = "CMZ", part2 = "MCD")
    )
