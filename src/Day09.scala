package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.ops._

object Day09 extends Day {

  sealed abstract class Direction(val dx: Int, val dy: Int)

  object Direction {

    case object Up    extends Direction(dx = 0, dy = 1)
    case object Down  extends Direction(dx = 0, dy = -1)
    case object Left  extends Direction(dx = -1, dy = 0)
    case object Right extends Direction(dx = 1, dy = 0)

    private val Line = """^([DLRU]) (\d+)$""".r

    def parse(line: String): List[Direction] =
      line match {
        case Line("U", count) => List.fill(count.toInt)(Up)
        case Line("D", count) => List.fill(count.toInt)(Down)
        case Line("L", count) => List.fill(count.toInt)(Left)
        case Line("R", count) => List.fill(count.toInt)(Right)
        case _                => Nil
      }

  }

  final case class Knot(x: Int, y: Int) {

    def position: (Int, Int) = (x, y)

    def move(dir: Direction): Knot =
      Knot(x + dir.dx, y + dir.dy)

    def pull(head: Knot): Knot = {
      val dx = (head.x - x)
      val dy = (head.y - y)

      if (dx.abs <= 1 && dy.abs <= 1)
        this
      else
        Knot(x + dx.clamp(-1, 1), y + dy.clamp(-1, 1))
    }

  }

  object Knot {

    val zero = Knot(0, 0)

  }

  final case class Rope(head: Knot, trail: List[Knot], visited: Set[(Int, Int)]) {

    def pull(direction: Direction): Rope = {
      @scala.annotation.tailrec
      def loop(head: Knot, trail: List[Knot], acc: List[Knot]): (List[Knot], Set[(Int, Int)]) =
        trail match {
          case trailHead :: trailTail =>
            val pulled = trailHead.pull(head)
            loop(pulled, trailTail, pulled :: acc)

          case Nil =>
            val visited = acc.headOption.map(_.position).toSet
            (acc.reverse, visited)
        }

      val newHead                = head.move(direction)
      val (newTrail, newVisited) = loop(newHead, trail, List.empty)

      Rope(newHead, newTrail, visited ++ newVisited)
    }

  }

  object Rope {

    def size(size: Int) =
      Rope(
        head = Knot.zero,
        trail = List.fill(size - 1)(Knot.zero),
        visited = Set(Knot.zero.position)
      )

  }

  def run(lines: LazyList[String]): Result = {
    val directions = lines.flatMap(Direction.parse)

    val part1 = directions.foldLeft(Rope.size(2))(_ pull _).visited.size
    val part2 = directions.foldLeft(Rope.size(10))(_ pull _).visited.size

    Result(part1.toString, part2.toString)
  }

}
