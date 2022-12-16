package dev.agjacome.aoc2022
package util

import scala.collection.immutable.Queue

object BFS {

  sealed trait Path { def visited: Set[Point] }

  object Path {
    final case class Found(path: List[Point], visited: Set[Point]) extends Path
    final case class NotFound(visited: Set[Point])                 extends Path
  }

  def apply(start: Point, end: Point, next: Point => Set[Point]): Path = {
    @scala.annotation.tailrec
    def loop(
        frontier: Queue[Point],
        cameFrom: Map[Point, Point]
    ): Path =
      frontier match {
        case `end` +: _ =>
          val path = reconstructPath(start, end, cameFrom, List.empty)
          Path.Found(path = path, visited = cameFrom.keySet)

        case current +: tail =>
          val nonVisited = next(current).filterNot(cameFrom.contains)
          loop(tail ++ nonVisited, cameFrom ++ nonVisited.map(_ -> current))

        case _ =>
          Path.NotFound(visited = cameFrom.keySet)
      }

    loop(Queue(start), Map(start -> start))
  }

  @scala.annotation.tailrec
  private def reconstructPath(
      start: Point,
      current: Point,
      cameFrom: Map[Point, Point],
      path: List[Point]
  ): List[Point] = {
    if (current == start)
      start :: path
    else
      reconstructPath(start, cameFrom(current), cameFrom, current :: path)
  }

}
