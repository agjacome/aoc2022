package dev.agjacome.aoc2022

object Day14 extends Day {

  sealed abstract class Material

  object Material {
    case object Rock extends Material
    case object Sand extends Material
  }

  final case class Cave(grid: Grid[Material], bottom: Int) {

    def +(kv: (Point, Material)): Cave =
      this.copy(grid = grid + kv)

    def withBottomAt(row: Int): Cave =
      this.copy(bottom = row)

    def isBlocked(p: Point): Boolean =
      fallPoint(p).isEmpty

    def count(mat: Material): Int =
      grid.cells.values.count(_ == mat)

    def percolate(src: Point, until: (Cave, Point) => Boolean): Cave = {
      @scala.annotation.tailrec
      def loop(curr: Point, cave: Cave): Cave =
        if (until(cave, curr)) {
          cave
        } else {
          cave.fallPoint(curr) match {
            case Some(dest) => loop(dest, cave)
            case None       => loop(src, cave + (curr -> Material.Sand))
          }
        }

      loop(src, this)
    }

    private def fallPoint(src: Point): Option[Point] =
      List(src.down, src.down.left, src.down.right)
        .filter(_.row <= bottom)
        .find(grid(_).isEmpty)

  }

  object Cave {

    val empty = Cave(Grid.empty, 0)

    def parse(lines: LazyList[String]): Cave = {
      val PointRegex = """^(\d+),(\d+)$""".r

      val rocks = lines.flatMap(
        _.split(" -> ")
          .collect { case PointRegex(col, row) => Point(row.toInt, col.toInt) }
          .sliding(2)
          .flatMap { case Array(src, dest) => src.lineTo(dest) }
      )

      val grid = rocks.foldLeft(Grid.empty[Material]) { case (grid, point) =>
        grid + (point -> Material.Rock)
      }

      Cave(grid, grid.height - 1)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val cave   = Cave.parse(lines)
    val source = Point(0, 500)

    val part1 = cave
      .percolate(source, until = (_, p) => p.row == cave.bottom)
      .count(Material.Sand)

    val part2 = cave
      .withBottomAt(cave.grid.height)
      .percolate(source, until = (c, p) => p == source && c.isBlocked(p))
      .count(Material.Sand) + 1

    Result(part1.toString, part2.toString)
  }

}
