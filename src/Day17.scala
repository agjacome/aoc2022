package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Indexed
import dev.agjacome.aoc2022.util.ops._

object Day17 extends Day {

  sealed trait Direction {
    def move(rows: Array[Byte]): Array[Byte]
  }

  object Direction {

    case object Down extends Direction {

      def move(rows: Array[Byte]): Array[Byte] =
        b"0000000" +: rows

    }

    case object Left extends Direction {

      def move(rows: Array[Byte]): Array[Byte] = {
        val overflows = rows.exists(b => (b & b"1000000") > 0)
        if (overflows) rows else rows.map(b => (b << 1).toByte)
      }

    }

    case object Right extends Direction {

      def move(rows: Array[Byte]): Array[Byte] = {
        val overflows = rows.exists(b => (b & b"0000001") > 0)
        if (overflows) rows else rows.map(b => (b >> 1).toByte)
      }

    }

    val fromSymbol: Char => Option[Direction] =
      Map('<' -> Left, '>' -> Right).get

  }

  final case class Rock(rows: Array[Byte]) extends AnyVal {

    def height: Int = rows.dropWhile(_ == 0).size

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

    private val trimmed = rows.dropWhile(_ == 0)
    private val height  = trimmed.size - 1

    private val columnPattern = {
      val columnHeights = trimmed
        .to(Seq)
        .map(_.toBitSeq)
        .transpose
        .flatMap(_.indexed.collectFirst { case Indexed(true, i) => height - i })

      val minHeight = columnHeights.min

      columnHeights.map(_ - minHeight)
    }

    def dropRocks(total: Long): Long = {
      type RockCount = Long
      type JetIndex  = Int
      type Jets      = LazyList[Indexed[Direction]]
      type RockIndex = Int
      type Rocks     = LazyList[Indexed[Rock]]
      type State     = Map[(RockIndex, JetIndex), (Chamber, RockCount)]

      @scala.annotation.tailrec
      def loop(
          count: RockCount,
          rocks: Rocks,
          directions: Jets,
          seen: State,
          current: Chamber
      ): Long =
        if (count == total)
          current.height.toLong
        else {
          val Indexed(rock, rockIndex) = rocks.head
          val Indexed(_, jetIndex)     = directions.head

          seen.get((rockIndex, jetIndex)) match {
            case Some((chamber, start)) if chamber.columnPattern == current.columnPattern =>
              val rocksPerCycle  = count - start
              val heightPerCycle = current.height - chamber.height
              val totalCycles    = total / rocksPerCycle

              val pendingRocks  = total % rocksPerCycle
              val pendingHeight = current.dropRocks(pendingRocks) - current.height

              totalCycles * heightPerCycle + pendingHeight

            case _ =>
              val (landed, nextDirs) = current
                .makeSpaceFor(rock)
                .moveUntilLanded(rock, directions)

              val newSeen = seen + ((rockIndex, jetIndex) -> (current, count))

              loop(count + 1, rocks.tail, nextDirs, newSeen, landed)
          }
        }

      val rocks = Rock.shapes.indexed.forever
      val dirs  = jets.indexed.forever
      loop(0, rocks, dirs, Map.empty, this)
    }

    private def makeSpaceFor(rock: Rock): Chamber = {
      val padding = Array.fill(rock.height + 3)(b"0000000")

      this.copy(rows = padding ++ trimmed)
    }

    @scala.annotation.tailrec
    private def moveUntilLanded(
        rock: Rock,
        directions: LazyList[Indexed[Direction]]
    ): (Chamber, LazyList[Indexed[Direction]]) =
      directions match {
        case Indexed(direction, _) #:: tail =>
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
      val moved  = Rock(direction.move(rock.rows))
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

  }

  object Chamber {

    def empty(jets: List[Direction]): Chamber =
      Chamber(rows = Array(b"1111111"), jets = jets)

  }

  def run(lines: LazyList[String]): Result = {
    val jets    = lines.flatMap(_.flatMap(Direction.fromSymbol)).to(List)
    val chamber = Chamber.empty(jets)

    val part1 = {
      val rockCount = 2022L
      chamber.dropRocks(rockCount)
    }

    val part2 = {
      val rockCount = 1_000_000_000_000L
      chamber.dropRocks(rockCount)
    }

    Result(part1.toString, part2.toString)
  }

}
