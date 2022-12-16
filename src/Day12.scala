package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.BFS
import dev.agjacome.aoc2022.util.Grid
import dev.agjacome.aoc2022.util.Point

object Day12 extends Day {

  final case class HeightMap(grid: Grid[Int], start: Point, end: Point) {

    lazy val adjacents: Map[Point, Set[Point]] =
      grid.adjacents.map { case (coordinate, adjacents) =>
        val elevation = grid(coordinate).getOrElse(-1)
        val filtered  = adjacents.filter { case (_, e) => e <= elevation + 1 }
        coordinate -> filtered.keySet
      }

    def +(kv: (Point, Int)): HeightMap =
      this.copy(grid = grid + kv)

    def withStart(square: Point): HeightMap =
      this.copy(start = square)

    def withEnd(square: Point): HeightMap =
      this.copy(end = square)

    def shortestPathFromStart: List[Point] =
      BFS(start, end, adjacents) match {
        case BFS.Path.Found(path, _) => path
        case BFS.Path.NotFound(_)    => Nil
      }

    def shortestPathFromElevation(elevation: Int): List[Point] =
      grid
        .collect((s, e) => Option.when(e == elevation)(s))
        .map(start => BFS(start, end, adjacents))
        .collect { case BFS.Path.Found(path, _) => path }
        .minBy(_.size)

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
          case chr :: tail => loopLine(acc + (square -> elevation(chr)), tail, square.right)
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
