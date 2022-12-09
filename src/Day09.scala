package dev.agjacome.aoc2022

sealed abstract class Direction(val dx: Int, val dy: Int)

object Direction {

  case object Up    extends Direction( 0,  1)
  case object Down  extends Direction( 0, -1)
  case object Left  extends Direction(-1,  0)
  case object Right extends Direction( 1,  0)

  def parse(line: String): List[Direction] = {
    val Line = """^([DLRU]) (\d+)$""".r

    line match {
      case Line("U", count) => List.fill(count.toInt)(Up)
      case Line("D", count) => List.fill(count.toInt)(Down)
      case Line("L", count) => List.fill(count.toInt)(Left)
      case Line("R", count) => List.fill(count.toInt)(Right)
      case _                => Nil
    }
  }

}

final case class Knot(x: Int, y: Int) {

  import Knot._

  def position: (Int, Int) = (x, y)

  def move(dir: Direction): Knot =
    Knot(x + dir.dx, y + dir.dy)

  def pull(head: Knot): Knot = {
    val dx = (head.x - this.x)
    val dy = (head.y - this.y)

    if (dx.abs <= 1 && dy.abs <= 1)
      this
    else
      Knot(x + dx.clamp(-1, 1), y + dy.clamp(-1, 1))
  }

}

object Knot {

  val zero = Knot(0, 0)

  implicit final class IntOps(private val self: Int) {
    def clamp(low: Int, high: Int): Int = self.max(low).min(high)
  }

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

    val newHead = head.move(direction)
    val (newTrail, newVisited) = loop(newHead, trail, List.empty)

    Rope(newHead, newTrail, visited ++ newVisited)
  }

}

object Rope {

  def size(size: Int) =
    Rope(
      head    = Knot.zero,
      trail   = List.fill(size - 1)(Knot.zero),
      visited = Set(Knot.zero.position)
    )

}

object Day09 extends Day {

  def run(lines: Iterator[String]): Result = {
    val motions = lines.flatMap(Direction.parse).to(LazyList)

    val part1 = motions.foldLeft(Rope.size( 2))(_ pull _).visited.size
    val part2 = motions.foldLeft(Rope.size(10))(_ pull _).visited.size

    Result(part1.toString, part2.toString)
  }

}
