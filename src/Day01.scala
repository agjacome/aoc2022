package dev.agjacome.aoc2022

object Day01 extends Day {

  def run(lines: LazyList[String]): Result = {
    val calories = lines
      .foldLeft(List.empty[Int]) {
        case (cals, "")          => 0 :: cals
        case (sum :: cals, line) => (line.toInt + sum) :: cals
        case (Nil, line)         => line.toInt :: Nil
      }
      .sorted(Ordering[Int].reverse)

    val part1 = calories.head
    val part2 = calories.take(3).sum

    Result(part1.toString, part2.toString)
  }

}
