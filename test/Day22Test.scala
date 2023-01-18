package dev.agjacome.aoc2022

final class Day22Test extends DayTest(Day22) {

  val input = LazyList(
    "        ...#",
    "        .#..",
    "        #...",
    "        ....",
    "...#.......#",
    "........#...",
    "..#....#....",
    "..........#.",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5"
  )

  val expected = Result("6032", "")

}
