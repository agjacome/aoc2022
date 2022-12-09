package dev.agjacome.aoc2022

final class Day09TestOne extends DayTest(
      day = Day09,
      testInputLines = List(
        "R 4",
        "U 4",
        "L 3",
        "D 1",
        "R 4",
        "D 1",
        "L 5",
        "R 2"
      ),
      expectedResult = Result(part1 = "13", part2 = "1")
    )

final class Day09TestTwo extends DayTest(
      day = Day09,
      testInputLines = List(
        "R 5",
        "U 8",
        "L 8",
        "D 3",
        "R 17",
        "D 10",
        "L 25",
        "U 20"
      ),
      expectedResult = Result(part1 = "88", part2 = "36")
    )
