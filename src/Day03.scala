package dev.agjacome.aoc2022

object Day03 extends Day {

  def repeated(cs: Seq[String]): Option[Char] =
    cs.reduceLeft(_ intersect _).headOption

  def priority(c: Char): Option[Int] = {
    def lowerBase = 'a'.toInt - 1
    def upperBase = 'A'.toInt - 27

    if (c.isLower)
      Some(c.toInt - lowerBase)
    else if (c.isUpper)
      Some(c.toInt - upperBase)
    else
      None
  }

  def run(lines: LazyList[String]): Result = {
    val part1 = lines
      .map(s => s.splitAt(s.length / 2))
      .flatMap(cs => repeated(List(cs._1, cs._2)))
      .flatMap(priority)
      .sum

    val part2 = lines
      .grouped(3)
      .flatMap(repeated)
      .flatMap(priority)
      .sum

    Result(part1.toString, part2.toString)
  }

}
