package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Point
import dev.agjacome.aoc2022.util.ops._

object Day09 extends Day {

  sealed abstract class Direction(val dx: Int, val dy: Int)

  object Direction {

    case object Up    extends Direction(dx = 0, dy = 1)
    case object Down  extends Direction(dx = 0, dy = -1)
    case object Left  extends Direction(dx = -1, dy = 0)
    case object Right extends Direction(dx = 1, dy = 0)

    private val Line = """^([DLRU]) (\d+)$""".r

    val parse: String => List[Direction] = {
      case Line("U", count) => List.fill(count.toInt)(Up)
      case Line("D", count) => List.fill(count.toInt)(Down)
      case Line("L", count) => List.fill(count.toInt)(Left)
      case Line("R", count) => List.fill(count.toInt)(Right)
      case line             => sys.error(s"Could not parse Direction: ${line}")
    }

  }

  final case class Knot(position: Point) extends AnyVal {

    def move(dir: Direction): Knot = {
      Knot(position + Point(dir.dy, dir.dx))
    }

    def pull(head: Knot): Knot = {
      val dx = (head.position.col - position.col)
      val dy = (head.position.row - position.row)

      if (dx.abs <= 1 && dy.abs <= 1)
        this
      else {
        val increment = Point(dy.clamp(-1, 1), dx.clamp(-1, 1))
        Knot(position + increment)
      }
    }

  }

  final case class Rope(head: Knot, trail: List[Knot], visited: Set[Point]) {

    def pull(direction: Direction): Rope = {
      @scala.annotation.tailrec
      def loop(head: Knot, trail: List[Knot], acc: List[Knot]): (List[Knot], Set[Point]) =
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
        head = Knot(Point.zero),
        trail = List.fill(size - 1)(Knot(Point.zero)),
        visited = Set(Point.zero)
      )

  }

  def run(lines: LazyList[String]): Result = {
    val directions = lines.flatMap(Direction.parse)

    val part1 = directions.foldLeft(Rope.size(2))(_ pull _).visited.size
    val part2 = directions.foldLeft(Rope.size(10))(_ pull _).visited.size

    Result(part1.toString, part2.toString)
  }

}
