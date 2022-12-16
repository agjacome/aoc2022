package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.ops._

object Day03 extends Day {

  def repeated(cs: Seq[String]): Option[Char] =
    cs.reduceLeft(_ intersect _).headOption

  def run(lines: LazyList[String]): Result = {
    val part1 = lines
      .map(s => s.splitAt(s.length / 2))
      .flatMap(cs => repeated(List(cs._1, cs._2)))
      .flatMap(_.toAlphabetInt)
      .sum

    val part2 = lines
      .grouped(3)
      .flatMap(repeated)
      .flatMap(_.toAlphabetInt)
      .sum

    Result(part1.toString, part2.toString)
  }

}
