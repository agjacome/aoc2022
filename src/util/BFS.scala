package dev.agjacome.aoc2022
package util

import scala.collection.immutable.Queue

object BFS {

  sealed trait Path[A] { def visited: Set[A] }

  object Path {
    final case class Found[A](path: List[A], visited: Set[A]) extends Path[A]
    final case class NotFound[A](visited: Set[A])             extends Path[A]
  }

  def apply[A](start: A, end: A, next: A => Set[A]): Path[A] = {
    @scala.annotation.tailrec
    def loop(
        frontier: Queue[A],
        cameFrom: Map[A, A]
    ): Path[A] =
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
  private def reconstructPath[A](
      start: A,
      current: A,
      cameFrom: Map[A, A],
      path: List[A]
  ): List[A] = {
    if (current == start)
      start :: path
    else
      reconstructPath(start, cameFrom(current), cameFrom, current :: path)
  }

}
