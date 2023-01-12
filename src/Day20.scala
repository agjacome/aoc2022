package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Indexed
import dev.agjacome.aoc2022.util.ops._

object Day20 extends Day {

  final case class Decrypter(key: Long, rounds: Int) {

    def extractCoordinates(numbers: Vector[Long]): Long = {
      val decrypted = decrypt(numbers)
      val zeroIndex = decrypted.indexOf(0L)

      Set(1000, 2000, 3000)
        .map(n => (zeroIndex + n) % decrypted.size)
        .map(decrypted.apply)
        .sum
    }

    def decrypt(numbers: Vector[Long]): Vector[Long] = {
      val original = numbers.map(_ * key).indexed

      @scala.annotation.tailrec
      def loop(rounds: Int, acc: Seq[Indexed[Long]]): Vector[Long] =
        if (rounds <= 0)
          acc.map(_.value).to(Vector)
        else
          loop(rounds - 1, mix(original, acc))

      loop(rounds, original)
    }

    private def mix(
        original: Seq[Indexed[Long]],
        numbers: Seq[Indexed[Long]]
    ): Seq[Indexed[Long]] =
      original.foldLeft(numbers) {
        case (moved, Indexed(0, _)) =>
          moved

        case (moved, idx @ Indexed(value, _)) =>
          val src = moved.indexOf(idx)
          val dst = dstIndex(src, value, numbers.size)

          moved.move(src, dst)
      }

    private def dstIndex(oldIndex: Int, value: Long, size: Int): Int = {
      val index = ((oldIndex + value) % (size - 1)).toInt

      if (index <= 0)
        index + size - 1
      else if (index >= size)
        index - size - 1
      else
        index
    }

  }

  def run(lines: LazyList[String]): Result = {
    val encrypted = lines.map(_.toLong).to(Vector)

    val part1 = {
      val decrypter = Decrypter(key = 1, rounds = 1)
      decrypter.extractCoordinates(encrypted)
    }

    val part2 = {
      val decrypter = Decrypter(key = 811589153, rounds = 10)
      decrypter.extractCoordinates(encrypted)
    }

    Result(part1.toString, part2.toString)
  }

}
