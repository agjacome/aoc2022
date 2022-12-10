package dev.agjacome.aoc2022

final class Day06Test extends DayTest(Day06) {

  val input = LazyList(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )

  val expected = Result(part1 = "7,5,6,10,11", part2 = "19,23,23,29,26")

}
