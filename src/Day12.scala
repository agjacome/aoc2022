package dev.agjacome.aoc2022

import scala.collection.immutable.Queue

final case class Square(row: Int, col: Int) {

  def up: Square    = this.copy(row = row - 1)
  def down: Square  = this.copy(row = row + 1)
  def left: Square  = this.copy(col = col - 1)
  def right: Square = this.copy(col = col + 1)

  def neighbors: Set[Square] = Set(up, down, left, right)

}

object Square {
  val zero: Square = Square(0, 0)
}

final case class HeightMap(
    elevations: Map[Square, Int],
    start: Square,
    end: Square
) {

  lazy val adjacents: Map[Square, Set[Square]] =
    elevations.map { case (source, elevation) =>
      val neighbors = source.neighbors.filter { destination =>
        elevations.get(destination).exists(_ <= elevation + 1)
      }

      source -> neighbors
    }

  def withElevation(square: Square, elevation: Int): HeightMap =
    this.copy(elevations = elevations + (square -> elevation))

  def withStart(square: Square): HeightMap =
    this.copy(start = square)

  def withEnd(square: Square): HeightMap =
    this.copy(end = square)

  def shortestPathFromStart: List[Square] =
    shortestPathFrom(start)

  def shortestPathsFrom(elevation: Int): Set[List[Square]] =
    elevations
      .collect { case (square, `elevation`) => square }
      .map(shortestPathFrom)
      .filter(_.nonEmpty)
      .toSet

  def allPaths: Set[List[Square]] =
    elevations.filter { case (_, e) => e == 1 }.keys.toSet.map(shortestPathFrom).filter(_.nonEmpty)

  // BFS, TODO: optimize to Dijkstra/A*
  private def shortestPathFrom(start: Square): List[Square] = {
    def reconstruct(
        cameFrom: Map[Square, Square],
        current: Square,
        path: List[Square]
    ): List[Square] = {
      if (current == start)
        start :: path
      else
        reconstruct(cameFrom, cameFrom(current), current :: path)
    }

    def loop(frontier: Queue[Square], cameFrom: Map[Square, Square]): List[Square] =
      frontier match {
        case `end` +: _ =>
          reconstruct(cameFrom, end, List.empty)

        case current +: tail =>
          val next = adjacents(current).filterNot(cameFrom.contains)
          loop(tail ++ next, cameFrom ++ next.map(_ -> current))

        case _ =>
          List.empty
      }

    loop(Queue(start), Map(start -> start))
  }

}

object HeightMap {

  val empty = HeightMap(elevations = Map.empty, start = Square.zero, end = Square.zero)

  def parse(lines: LazyList[String]): HeightMap = {
    def elevation(c: Char): Int = {
      val base = 'a'.toInt - 1
      if (c.isLower) c.toInt - base else 0
    }

    @scala.annotation.tailrec
    def loopLine(acc: HeightMap, line: List[Char], square: Square): HeightMap =
      line match {
        case Nil         => acc
        case 'S' :: tail => loopLine(acc.withStart(square), 'a' :: tail, square)
        case 'E' :: tail => loopLine(acc.withEnd(square), 'z' :: tail, square)
        case chr :: tail => loopLine(acc.withElevation(square, elevation(chr)), tail, square.right)
      }

    @scala.annotation.tailrec
    def loop(acc: HeightMap, lines: LazyList[String], row: Int): HeightMap =
      lines match {
        case LazyList()    => acc
        case line #:: tail => loop(loopLine(acc, line.toList, Square(row, 0)), tail, row + 1)
      }

    loop(acc = HeightMap.empty, lines = lines, row = 0)
  }

}

object Day12 extends Day {

  def run(lines: LazyList[String]): Result = {
    val map = HeightMap.parse(lines)

    val part1 = map.shortestPathFromStart.size - 1
    val part2 = map.shortestPathsFrom(elevation = 1).map(_.size - 1).min

    Result(part1.toString, part2.toString)
  }

}
