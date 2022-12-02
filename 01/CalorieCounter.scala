#!/usr/bin/env scala

import scala.io.Source

object Day1 extends App{

  val source = args.headOption.fold(Source.stdin)(Source.fromFile)

  val calories = source.getLines
    .foldLeft(List.empty[Int]) {
      case (cals       , "")   => 0 :: cals
      case (sum :: cals, line) => (line.toInt + sum) :: cals
      case (Nil        , line) => line.toInt :: Nil
    }
    .sorted(Ordering[Int].reverse)

  println(s"Part 1: ${calories.head}")
  println(s"Part 2: ${calories.take(3).sum}")

}
