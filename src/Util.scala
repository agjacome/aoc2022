package dev.agjacome.aoc2022

final case class Coordinates(row: Int, col: Int) {

  def up: Coordinates    = this.copy(row = row - 1)
  def down: Coordinates  = this.copy(row = row + 1)
  def left: Coordinates  = this.copy(col = col - 1)
  def right: Coordinates = this.copy(col = col + 1)

  def neighbors: Set[Coordinates] = Set(up, down, left, right)

}

object Coordinates {
  val zero: Coordinates = Coordinates(0, 0)
}

final case class Grid[A](cells: Map[Coordinates, A]) {

  lazy val rows: Map[Int, Map[Int, A]] = {
    val group: ((Coordinates, A)) => Int       = { case (coord, _) => coord.row }
    val map: ((Coordinates, A)) => Map[Int, A] = { case (coord, cell) => Map(coord.col -> cell) }
    val reduce: (Map[Int, A], Map[Int, A]) => Map[Int, A] = _ ++ _

    cells.groupMapReduce(group)(map)(reduce)
  }

  lazy val columns: Map[Int, Map[Int, A]] = {
    val group: ((Coordinates, A)) => Int       = { case (coord, _) => coord.col }
    val map: ((Coordinates, A)) => Map[Int, A] = { case (coord, cell) => Map(coord.row -> cell) }
    val reduce: (Map[Int, A], Map[Int, A]) => Map[Int, A] = _ ++ _

    cells.groupMapReduce(group)(map)(reduce)
  }

  lazy val adjacents: Map[Coordinates, Map[Coordinates, A]] =
    cells.map { case (coordinate, _) =>
      val neighbors: Map[Coordinates, A] = coordinate.neighbors
        .filter(cells.contains)
        .flatMap(coord => this(coord).map(coord -> _))
        .toMap

      coordinate -> neighbors
    }

  def apply(coord: Coordinates): Option[A] =
    cells.get(coord)

  def apply(row: Int, column: Int): Option[A] =
    cells.get(Coordinates(row, column))

  def +(v: (Coordinates, A)): Grid[A] =
    this.copy(cells = cells + v)

  def map[B](f: (Coordinates, A) => B): List[B] =
    cells.map(f.tupled).to(List)

  def filter(f: (Coordinates, A) => Boolean): Map[Coordinates, A] =
    cells.filter(f.tupled)

  def collect[B](f: (Coordinates, A) => Option[B]): List[B] =
    cells.collect(f.tupled.unlift).to(List)

  def row(row: Int): List[A] =
    rows.getOrElse(row, Map.empty).to(List).sortBy(_._1).map(_._2)

  def column(col: Int): List[A] =
    columns.getOrElse(col, Map.empty).to(List).sortBy(_._1).map(_._2)

  def topView(coord: Coordinates): List[A] =
    column(coord.col).take(coord.row).reverse

  def leftView(coord: Coordinates): List[A] =
    row(coord.row).take(coord.col).reverse

  def bottomView(coord: Coordinates): List[A] =
    column(coord.col).drop(coord.row + 1)

  def rightView(coord: Coordinates): List[A] =
    row(coord.row).drop(coord.col + 1)

}

object Grid {

  def empty[A]: Grid[A] = Grid(Map.empty)

  def fromRows[A](rows: Seq[Seq[A]]): Grid[A] = {
    val indexed: Seq[(Coordinates, A)] = rows.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.map { case (cell, j) =>
        Coordinates(i, j) -> cell
      }
    }

    Grid(indexed.toMap)
  }

}
