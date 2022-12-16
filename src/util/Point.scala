package dev.agjacome.aoc2022
package util

final case class Point(row: Int, col: Int) {

  def up: Point    = this.copy(row = row - 1)
  def down: Point  = this.copy(row = row + 1)
  def left: Point  = this.copy(col = col - 1)
  def right: Point = this.copy(col = col + 1)

  def neighbors: Set[Point] = Set(up, down, left, right)

  def isUpOf(that: Point): Boolean    = this.row < that.row && this.col == that.col
  def isDownOf(that: Point): Boolean  = this.row > that.row && this.col == that.col
  def isLeftOf(that: Point): Boolean  = this.col < that.col && this.row == that.row
  def isRightOf(that: Point): Boolean = this.col > that.col && this.row == that.row

  def lineTo(dest: Point): List[Point] = {
    @scala.annotation.tailrec
    def loop(curr: Point, acc: List[Point]): List[Point] =
      if (curr == dest)
        (dest :: acc).reverse
      else if (curr.isUpOf(dest))
        loop(curr.down, curr :: acc)
      else if (curr.isDownOf(dest))
        loop(curr.up, curr :: acc)
      else if (curr.isLeftOf(dest))
        loop(curr.right, curr :: acc)
      else if (curr.isRightOf(dest))
        loop(curr.left, curr :: acc)
      else
        Nil

    loop(this, Nil)
  }

  def distanceTo[A](that: Point, metric: Point.DistanceMetric[A]): A = {
    import Point.DistanceMetric._

    metric match {
      case Euclidean =>
        Math.sqrt(
          Math.pow((this.row - that.row).toDouble, 2) +
            Math.pow((this.col - that.col).toDouble, 2)
        )

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
