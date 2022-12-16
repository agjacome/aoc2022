package dev.agjacome.aoc2022
package util

final case class Interval(low: Int, high: Int) {

  lazy val size: Int = (high - low).abs

  def overlaps(that: Interval): Boolean =
    Math.max(this.low, that.low) <= Math.min(this.high, that.high)

  def borders(that: Interval): Boolean =
    Math.max(this.low, that.low) == Math.min(this.high, that.high) + 1

  def union(that: Interval): Option[Interval] =
    Option.when(this.overlaps(that) || this.borders(that)) {
      val low  = Math.min(this.low, that.low)
      val high = Math.max(this.high, that.high)
      Interval(low, high)
    }

}

object Interval {

  implicit val ord: Ordering[Interval] = Ordering.by(_.low)

  def disjointUnion(intervals: Set[Interval]): Set[Interval] = {
    @scala.annotation.tailrec
    def loop(prev: Interval, intervals: List[Interval], acc: Set[Interval]): Set[Interval] =
      intervals match {
        case Nil => acc + prev
        case curr :: next =>
          prev.union(curr) match {
            case Some(union) => loop(union, next, acc)
            case None        => loop(curr, next, acc + prev)
          }
      }

    intervals.to(List).sorted match {
      case head :: tail => loop(head, tail, Set.empty)
      case Nil          => Set.empty
    }
  }

}
