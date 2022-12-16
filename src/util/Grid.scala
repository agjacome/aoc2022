package dev.agjacome.aoc2022
package util

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

  def keys: Set[Point] =
    cells.keySet

  def values: List[A] =
    cells.values.to(List)

  def +(v: (Point, A)): Grid[A] =
    this.copy(cells = cells + v)

  def map[B](f: (Point, A) => B): List[B] =
    cells.map(f.tupled).to(List)

  def map[B](f: (Point, A) => (Point, B)): Grid[B] =
    this.copy(cells = cells.map(f.tupled))

  def filter(f: (Point, A) => Boolean): Grid[A] =
    this.copy(cells = cells.filter(f.tupled))

  def collect[B](f: (Point, A) => Option[B]): List[B] =
    cells.collect(f.tupled.unlift).to(List)

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
