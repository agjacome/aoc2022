package dev.agjacome.aoc2022
package util

object ops {

  implicit final class CharOps(private val self: Char) extends AnyVal {

    def toAlphabetInt: Option[Int] = {
      val lowerBase = 'a'.toInt - 1
      val upperBase = 'A'.toInt - 27

      if (self.isLower)
        Some(self.toInt - lowerBase)
      else if (self.isUpper)
        Some(self.toInt - upperBase)
      else
        None
    }

  }

  implicit final class OrderedOps[A: Ordering](private val self: A) {

    def clamp(low: A, high: A): A =
      A.min(A.max(self, low), high)

  }

  implicit final class SeqOps[A](private val self: Seq[A]) extends AnyVal {

    def takeUntil(f: A => Boolean): Seq[A] =
      self.span(f) match {
        case (head, tail) => head ++ tail.take(1)
      }

  }

  implicit final class MapOps[A, B](private val self: Map[A, B]) extends AnyVal {

    def updating(k: A)(f: B => B): Map[A, B] =
      self.updatedWith(k)(_.map(f))

  }

}
