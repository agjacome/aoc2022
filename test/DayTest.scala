package dev.agjacome.aoc2022

import munit.FunSuite

abstract class DayTest(day: Day) extends FunSuite {

  protected def input: LazyList[String]
  protected def expected: Result

  test("part 1") {
    val actual = day.run(input)
    assertEquals(actual.part1, expected.part1)
  }

  test("part 2") {
    val actual = day.run(input)
    assertEquals(actual.part2, expected.part2)
  }

}
