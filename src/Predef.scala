package dev.agjacome.aoc2022

import scala.collection.immutable.SortedMap

object predef {

  implicit final class OrderedOps[A: Ordering](private val self: A) {

    private val A = Ordering[A]

    def clamp(low: A, high: A): A =
      A.min(A.max(self, low), high)

  }

  implicit final class SeqOps[A](private val self: Seq[A]) extends AnyVal {

    def takeUntil(f: A => Boolean): Seq[A] =
      self.span(f) match {
        case (head, tail) => head ++ tail.take(1)
      }

  }

  implicit final class SortedMapOps[A, B](private val self: SortedMap[A, B]) extends AnyVal {
    def update(k: A)(f: B => B): SortedMap[A, B] =
      self.updatedWith(k)(_.map(f))
  }

}
