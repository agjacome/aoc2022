package dev.agjacome.aoc2022

object Day01 extends Day {

  def run(lines: Iterator[String]): Result = {
    val calories = lines
      .foldLeft(List.empty[Int]) {
        case (cals, "")          => 0 :: cals
        case (sum :: cals, line) => (line.toInt + sum) :: cals
        case (Nil, line)         => line.toInt :: Nil
      }
      .sorted(Ordering[Int].reverse)

    Result(
      part1 = calories.head.toString,
      part2 = calories.take(3).sum.toString
    )
  }

}
