package dev.agjacome.aoc2022
package util

final case class Point(row: Int, col: Int) {

  def +(that: Point): Point =
    Point(this.row + that.row, this.col + that.col)

  def up: Point    = this.copy(row = row - 1)
  def down: Point  = this.copy(row = row + 1)
  def left: Point  = this.copy(col = col - 1)
  def right: Point = this.copy(col = col + 1)

  def neighbors: Set[Point] = Set(up, down, left, right)

  def lineTo(dest: Point): List[Point] = {
    @scala.annotation.tailrec
    def loop(curr: Point, acc: List[Point]): List[Point] =
      if (curr == dest) {
        (dest :: acc).reverse
      } else if (curr.col == dest.col) {
        val next = if (curr.row < dest.row) curr.down else curr.up
        loop(next, curr :: acc)
      } else if (curr.row == dest.row) {
        val next = if (curr.col < dest.col) curr.right else curr.left
        loop(next, curr :: acc)
      } else {
        Nil
      }

    loop(this, Nil)
  }

  def distanceTo[A](that: Point, metric: Point.DistanceMetric[A]): A = {
    import java.lang.Math._
    import Point.DistanceMetric._

    metric match {
      case Euclidean =>
        sqrt(pow((this.row - that.row).toDouble, 2) + pow((this.col - that.col).toDouble, 2))

      case Manhattan =>
        (this.row - that.row).abs + (this.col - that.col).abs
    }
  }

}

object Point {

  sealed trait DistanceMetric[A]

  object DistanceMetric {
    case object Euclidean extends DistanceMetric[Double]
    case object Manhattan extends DistanceMetric[Int]
  }

  implicit val order: Ordering[Point] =
    Ordering.by(coord => (coord.row, coord.col))

  val zero: Point = Point(0, 0)

}
