package dev.agjacome.aoc2022

import scala.collection.immutable.Queue

object Day12 extends Day {

  final case class HeightMap(grid: Grid[Int], start: Point, end: Point) {

    lazy val adjacents: Map[Point, Set[Point]] =
      grid.adjacents.map { case (coordinate, adjacents) =>
        val elevation = grid(coordinate).getOrElse(-1)
        val filtered  = adjacents.filter { case (_, e) => e <= elevation + 1 }
        coordinate -> filtered.keySet
      }

    def withElevation(square: Point, elevation: Int): HeightMap =
      this.copy(grid = grid + (square -> elevation))

    def withStart(square: Point): HeightMap =
      this.copy(start = square)

    def withEnd(square: Point): HeightMap =
      this.copy(end = square)

    def shortestPathFromStart: List[Point] =
      bfs(start, end)

    def shortestPathFromElevation(elevation: Int): List[Point] = {
      grid
        .collect((s, e) => Option.when(e == elevation)(s))
        .map(start => bfs(start, end))
        .filter(_.nonEmpty)
        .minBy(_.size)
    }

    private def bfs(start: Point, end: Point): List[Point] = {
      @scala.annotation.tailrec
      def reconstruct(
          cameFrom: Map[Point, Point],
          current: Point,
          path: List[Point]
      ): List[Point] = {
        if (current == start)
          start :: path
        else
          reconstruct(cameFrom, cameFrom(current), current :: path)
      }

      @scala.annotation.tailrec
      def loop(
          frontier: Queue[Point],
          cameFrom: Map[Point, Point]
      ): List[Point] =
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

    val empty = HeightMap(grid = Grid.empty[Int], start = Point.zero, end = Point.zero)

    def parse(lines: LazyList[String]): HeightMap = {
      def elevation(c: Char): Int = {
        val base = 'a'.toInt - 1
        if (c.isLower) c.toInt - base else 0
      }

      @scala.annotation.tailrec
      def loopLine(acc: HeightMap, line: List[Char], square: Point): HeightMap =
        line match {
          case Nil         => acc
          case 'S' :: tail => loopLine(acc.withStart(square), 'a' :: tail, square)
          case 'E' :: tail => loopLine(acc.withEnd(square), 'z' :: tail, square)
          case chr :: tail =>
            loopLine(acc.withElevation(square, elevation(chr)), tail, square.right)
        }

      @scala.annotation.tailrec
      def loop(acc: HeightMap, lines: LazyList[String], row: Int): HeightMap =
        lines match {
          case LazyList()    => acc
          case line #:: tail => loop(loopLine(acc, line.toList, Point(row, 0)), tail, row + 1)
        }

      loop(acc = HeightMap.empty, lines = lines, row = 0)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val map = HeightMap.parse(lines)

    val part1 = map.shortestPathFromStart.size - 1
    val part2 = map.shortestPathFromElevation(1).size - 1

    Result(part1.toString, part2.toString)
  }

}
