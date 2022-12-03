#!/usr/bin/env scala

import scala.io.Source

object Day3 extends App {

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

  val source = args.headOption.fold(Source.stdin)(Source.fromFile)
  val lines  = source.getLines.to(LazyList)

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

  println(s"Part 1: ${part1}")
  println(s"Part 2: ${part2}")

}
