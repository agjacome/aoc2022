package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.ops._

object Day17 extends Day {

  implicit final class BinaryByteInterpolator(private val sc: StringContext) extends AnyVal {

    def b(@scala.annotation.unused args: Any*): Byte = {
      val string = sc.parts.iterator.mkString
      java.lang.Byte.parseByte(string, 2)
    }

  }

  sealed abstract class Direction(val move: Array[Byte] => Array[Byte])

  object Direction {

    case object Down  extends Direction(b"0000000" +: _)
    case object Left  extends Direction(_.map(b => if ((b & b"1000000") > 0) b else (b << 1).toByte))
    case object Right extends Direction(_.map(b => if ((b & b"0000001") > 0) b else (b >> 1).toByte))

    val fromSymbol: Char => Option[Direction] =
      Map('<' -> Left, '>' -> Right).get

  }

  final case class Rock(rows: Array[Byte]) extends AnyVal {

    def height = rows.size

    def move(direction: Direction): Rock = {
      val moved = direction.move(rows)
      if (moved.forall(_ >= 0)) Rock(moved) else this
    }

  }

  object Rock {

    def apply(rows: Byte*): Rock = Rock(rows.to(Array))

    val shapes = List(
      Rock(b"0011110"),                                     // −
      Rock(b"0001000", b"0011100", b"0001000"),             // +
      Rock(b"0000100", b"0000100", b"0011100"),             // ⌋
      Rock(b"0010000", b"0010000", b"0010000", b"0010000"), // |
      Rock(b"0011000", b"0011000")                          // □
    )

  }

  final case class Chamber(rows: Array[Byte], jets: List[Direction]) {

    def towerHeight: Int =
      rows.dropWhile(_ == 0).size - 1

    def dropRocks(count: Int): Chamber = {
      @scala.annotation.tailrec
      def loop(
        countdown: Int,
        rocks: LazyList[Rock],
        directions: LazyList[Direction],
        chamber: Chamber
      ): Chamber = {
        println(s"${chamber}\n")
        (countdown, rocks) match {
          case (0, _) | (_, LazyList()) =>
            chamber

          case (_, rock #:: nextRocks) =>
            val (nextChamber, nextDirs) = chamber.pad.moveUntilLanded(rock, directions)
            loop(countdown - 1, nextRocks, nextDirs, nextChamber)
        }
      }

      loop(count, Rock.shapes.forever, jets.forever, this)
    }

    def pad: Chamber = {
      val topRock = rows.indexWhere(_ != 0)
      val padding = Array.fill(4 - topRock)(b"0000000")

      this.copy(rows = padding ++ rows)
    }

    @scala.annotation.tailrec
    private def moveUntilLanded(rock: Rock, directions: LazyList[Direction]): (Chamber, LazyList[Direction]) =
      directions match {
        case direction #:: tail =>
          val movedByJet     = move(rock, direction)
          val movedByGravity = move(movedByJet, Direction.Down)

          val hasLanded = movedByGravity == movedByJet
          if (hasLanded) {
            (this merge movedByGravity, tail)
          } else {
            moveUntilLanded(movedByGravity, tail)
          }

        case _ => (this merge rock, directions)
      }

    private def move(rock: Rock, direction: Direction): Rock = {
      val moved  = rock.move(direction)
      val zipped = this.zipWith(moved)

      val hasCollision = zipped.exists { case (x, y) => (x & y) > 0 }

      if (hasCollision) rock else moved
    }

    private def merge(rock: Rock): Chamber = {
      val zipped = this.zipWith(rock)
      val merged = zipped.map { case (x, y) => (x | y).toByte }

      this.copy(rows = merged)
    }

    private def zipWith(rock: Rock): Array[(Byte, Byte)] =
      this.rows.zipAll(rock.rows, b"0000000", b"0000000")

    override def toString: String = {
      def formatRow(b: Byte): String =
        f"|${b.toInt.toBinaryString}%7s|"
          .replace('0', '.')
          .replace(' ', '.')
          .replace('1', '#')

      (rows.dropRight(1).map(formatRow) :+ "+-------+").mkString("\n")
    }

  }

  object Chamber {

    def empty(jets: List[Direction]): Chamber =
      Chamber(rows = Array(b"1111111"), jets = jets)

  }

  def run(lines: LazyList[String]): Result = {
    val jets = lines.flatMap(_.flatMap(Direction.fromSymbol)).to(List)

    val _ = Chamber.empty(jets).dropRocks(3)

    ???
  }

}
