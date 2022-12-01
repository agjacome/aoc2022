#!/usr/bin/env scala

import scala.io.Source

object Day1 extends App{

  val source = args.headOption.fold(Source.stdin)(Source.fromFile)

  val calories = source.getLines
    .foldLeft(List.empty[List[Int]]) {
      case (acc, "")         => Nil :: acc
      case (head :: tail, x) => (x.toInt :: head) :: tail
      case (Nil, x)          => (x.toInt :: Nil) :: Nil
    }
    .map(_.sum)
    .sorted(Ordering[Int].reverse)

  println(s"Part 1: ${calories.headOption.getOrElse(0)}")
  println(s"Part 2: ${calories.take(3).sum}")

}
