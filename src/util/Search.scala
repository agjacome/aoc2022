package dev.agjacome.aoc2022
package util

import scala.collection.immutable.Queue

trait Search {

  def apply[A](start: A, end: A, next: A => Set[A]): Search.Path[A] =
    this(start, _ == end, next)

  def apply[A](start: A, isEnd: A => Boolean, next: A => Set[A]): Search.Path[A]

}

object Search {

  sealed trait Path[A] { def visited: Set[A] }

  object Path {

    final case class Found[A](path: List[A], visited: Set[A]) extends Path[A]
    final case class NotFound[A](visited: Set[A])             extends Path[A]

    @scala.annotation.tailrec
    def reconstruct[A](start: A, current: A, cameFrom: Map[A, A], path: List[A]): Path[A] = {
      if (current == start)
        Path.Found(path = start :: path, visited = cameFrom.keySet)
      else
        reconstruct(start, cameFrom(current), cameFrom, current :: path)
    }

  }

  object DFS extends Search {

    override def apply[A](start: A, isEnd: A => Boolean, next: A => Set[A]): Path[A] = {
      @scala.annotation.tailrec
      def loop(stack: List[A], discovered: Map[A, A]): Path[A] =
        stack match {
          case end :: _ if isEnd(end) =>
            Path.reconstruct(start, end, discovered, List.empty)

          case current :: tail =>
            val visited    = discovered.keySet
            val nonVisited = next(current) diff visited
            loop(nonVisited.to(List) ::: tail, discovered ++ nonVisited.map(_ -> current))

          case _ =>
            Path.NotFound(visited = discovered.keySet)
        }

      loop(List(start), Map(start -> start))
    }

  }

  object BFS extends Search {

    override def apply[A](start: A, isEnd: A => Boolean, next: A => Set[A]): Path[A] = {
      @scala.annotation.tailrec
      def loop(frontier: Queue[A], cameFrom: Map[A, A]): Path[A] =
        frontier match {
          case end +: _ if isEnd(end) =>
            Path.reconstruct(start, end, cameFrom, List.empty)

          case current +: tail =>
            val visited    = cameFrom.keySet
            val nonVisited = next(current) diff visited
            loop(tail ++ nonVisited, cameFrom ++ nonVisited.map(_ -> current))

          case _ =>
            Path.NotFound(visited = cameFrom.keySet)
        }

      loop(Queue(start), Map(start -> start))
    }

  }

}
