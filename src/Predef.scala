package dev.agjacome.aoc2022

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

}
