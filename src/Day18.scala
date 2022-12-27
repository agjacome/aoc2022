package dev.agjacome.aoc2022

import scala.collection.immutable.Queue

import dev.agjacome.aoc2022.util.ops._

object Day18 extends Day {

  final case class Point3D(x: Int, y: Int, z: Int) {

    def +(that: Point3D): Point3D =
      Point3D(
        x = this.x + that.x,
        y = this.y + that.y,
        z = this.z + that.z
      )

    def neighbors: Set[Point3D] = {
      val deltas = Set(
        Point3D(-1, 0, 0),
        Point3D(0, -1, 0),
        Point3D(0, 0, -1),
        Point3D(0, 0, 1),
        Point3D(0, 1, 0),
        Point3D(1, 0, 0)
      )

      deltas.map(this + _)
    }

    def between(min: Point3D, max: Point3D): Boolean =
      x.between(min.x, max.x) && y.between(min.y, max.y) && z.between(min.z, max.z)

  }

  object Point3D {

    val zero: Point3D = Point3D(0, 0, 0)

    def parse(line: String): Option[Point3D] = {
      val Point3DLine = """(\d+),(\d+),(\d+)""".r

      line match {
        case Point3DLine(x, y, z) => Some(Point3D(x.toInt, y.toInt, z.toInt))
        case _                    => None
      }
    }

  }

  final case class Pond(droplets: Set[Point3D]) {

    def surfaceArea: Int =
      droplets.foldLeft(0) { case (sum, droplet) =>
        val neighbors = droplet.neighbors.filter(droplets.contains)
        sum + 6 - neighbors.size
      }

    def exteriorSurfaceArea: Int = {
      @scala.annotation.tailrec
      def loop(frontier: Queue[Point3D], visited: Set[Point3D], count: Int): Int = {
        frontier match {
          case `max` +: _ =>
            count

          case point +: tail =>
            val neighbors = point.neighbors
              .filterNot(visited.contains)
              .filterNot(frontier.contains)
              .filter(_.between(min, max))

            val (exterior, next) = neighbors.partition(droplets.contains)

            loop(tail ++ next, visited + point, count + exterior.size)

          case _ => count
        }
      }

      loop(Queue(min), Set.empty, 0)
    }

    private val min: Point3D = {
      val minX = droplets.map(_.x).min - 1
      val minY = droplets.map(_.y).min - 1
      val minZ = droplets.map(_.z).min - 1

      Point3D(minX, minY, minZ)
    }

    private val max: Point3D = {
      val maxX = droplets.map(_.x).max + 1
      val maxY = droplets.map(_.y).max + 1
      val maxZ = droplets.map(_.z).max + 1

      Point3D(maxX, maxY, maxZ)
    }

  }

  object Pond {

    def parse(lines: Seq[String]): Pond =
      Pond(lines.flatMap(Point3D.parse).to(Set))

  }

  def run(lines: LazyList[String]): Result = {
    val pond = Pond.parse(lines)

    val part1 = pond.surfaceArea
    val part2 = pond.exteriorSurfaceArea

    Result(part1.toString, part2.toString)
  }

}
