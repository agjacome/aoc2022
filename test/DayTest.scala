package dev.agjacome.aoc2022

import munit.FunSuite

abstract class DayTest(
  day: Day,
  testInputLines: List[String],
  expectedResult: Result
) extends FunSuite {

  test("part 1") {
    val expected = expectedResult.part1
    val actual   = day.run(testInputLines.iterator)

    assertEquals(actual.part1, expected)
  }

  test("part 2") {
    val expected = expectedResult.part2
    val actual   = day.run(testInputLines.iterator)

    assertEquals(actual.part2, expected)
  }

}

