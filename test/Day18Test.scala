package dev.agjacome.aoc2022

final class Day18Test extends DayTest(Day18) {

  val input = LazyList(
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5"
  )

  val expected = Result("64", "58")

}
