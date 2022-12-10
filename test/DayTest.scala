package dev.agjacome.aoc2022

import munit.FunSuite

abstract class DayTest(day: Day) extends FunSuite {

  protected def input: LazyList[String]
  protected def expected: Result

  private lazy val actual = day.run(input)

  test("part 1") {
    assertEquals(actual.part1, expected.part1)
  }

  test("part 2") {
    assertEquals(actual.part2, expected.part2)
  }

}
