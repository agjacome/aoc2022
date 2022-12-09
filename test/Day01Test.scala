package dev.agjacome.aoc2022

final class Day01Test extends DayTest(
      day = Day01,
      testInputLines = List(
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
      ),
      expectedResult = Result(part1 = "24000", part2 = "45000")
    )
