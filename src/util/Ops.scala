package dev.agjacome.aoc2022
package util

import scala.math.Ordering.Implicits._

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

  implicit final class ByteOps(private val self: Byte) extends AnyVal {

    def toBitSeq: Seq[Boolean] =
      (0 to 7).map(bit => ((self >> bit) & 1) == 1)

  }

  implicit final class OrderedOps[A: Ordering](private val self: A) {

    def between(low: A, high: A): Boolean =
      low <= self && self <= high

    def clamp(low: A, high: A): A =
      self.max(low).min(high)

  }

  implicit final class SeqOps[A](private val self: Seq[A]) extends AnyVal {

    def indexed: Seq[Indexed[A]] =
      self.zipWithIndex.map(Function.tupled(Indexed.apply))

    def takeUntil(f: A => Boolean): Seq[A] =
      self.span(f) match {
        case (head, tail) => head ++ tail.take(1)
      }

    def forever: LazyList[A] =
      LazyList.continually(self).flatten

  }

  implicit final class MapOps[A, B](private val self: Map[A, B]) extends AnyVal {

    def updating(k: A)(f: B => B): Map[A, B] =
      self.updatedWith(k)(_.map(f))

  }

  implicit final class BinaryByteInterpolator(private val sc: StringContext) extends AnyVal {

    def b(@scala.annotation.unused args: Any*): Byte = {
      val string = sc.parts.iterator.mkString
      java.lang.Byte.parseByte(string, 2)
    }

  }

}
