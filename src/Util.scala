package dev.agjacome.aoc2022

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

final case class Grid[A](cells: Map[Point, A]) {

  lazy val height: Int = cells.keySet.map(_.row).max + 1
  lazy val width: Int  = cells.keySet.map(_.col).max + 1

  lazy val rows: Map[Int, Map[Int, A]] = {
    val group: ((Point, A)) => Int       = { case (coord, _) => coord.row }
    val map: ((Point, A)) => Map[Int, A] = { case (coord, cell) => Map(coord.col -> cell) }
    val reduce: (Map[Int, A], Map[Int, A]) => Map[Int, A] = _ ++ _

    cells.groupMapReduce(group)(map)(reduce)
  }

  lazy val columns: Map[Int, Map[Int, A]] = {
    val group: ((Point, A)) => Int       = { case (coord, _) => coord.col }
    val map: ((Point, A)) => Map[Int, A] = { case (coord, cell) => Map(coord.row -> cell) }
    val reduce: (Map[Int, A], Map[Int, A]) => Map[Int, A] = _ ++ _

    cells.groupMapReduce(group)(map)(reduce)
  }

  lazy val adjacents: Map[Point, Map[Point, A]] =
    cells.map { case (coordinate, _) =>
      val neighbors: Map[Point, A] = coordinate.neighbors
        .filter(cells.contains)
        .flatMap(coord => this(coord).map(coord -> _))
        .toMap

      coordinate -> neighbors
    }

  def apply(coord: Point): Option[A] =
    cells.get(coord)

  def +(v: (Point, A)): Grid[A] =
    this.copy(cells = cells + v)

  def map[B](f: (Point, A) => B): List[B] =
    cells.map(f.tupled).to(List)

  def collect[B](f: (Point, A) => Option[B]): List[B] =
    cells.collect(f.tupled.unlift).to(List)

  def row(row: Int): List[A] =
    rows.getOrElse(row, Map.empty).to(List).sortBy(_._1).map(_._2)

  def column(col: Int): List[A] =
    columns.getOrElse(col, Map.empty).to(List).sortBy(_._1).map(_._2)

  def topView(coord: Point): List[A] =
    column(coord.col).take(coord.row).reverse

  def leftView(coord: Point): List[A] =
    row(coord.row).take(coord.col).reverse

  def bottomView(coord: Point): List[A] =
    column(coord.col).drop(coord.row + 1)

  def rightView(coord: Point): List[A] =
    row(coord.row).drop(coord.col + 1)

}

object Grid {

  def empty[A]: Grid[A] = Grid(Map.empty)

  def fromRows[A](rows: Seq[Seq[A]]): Grid[A] = {
    val indexed: Seq[(Point, A)] = rows.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.map { case (cell, j) =>
        Point(i, j) -> cell
      }
    }

    Grid(indexed.toMap)
  }

}
