package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Grid
import dev.agjacome.aoc2022.util.Point
import dev.agjacome.aoc2022.util.ops._

object Day08 extends Day {

  type TreeHeight  = Int
  type ScenicScore = Int

  final case class Forest(trees: Grid[TreeHeight]) extends AnyVal {

    def visibleTrees: List[TreeHeight] = {
      def isVisible(p: Point, h: TreeHeight) = {
        def top    = topView(p).forall(_ < h)
        def left   = leftView(p).forall(_ < h)
        def bottom = bottomView(p).forall(_ < h)
        def right  = rightView(p).forall(_ < h)

        top || left || bottom || right
      }

      trees.filter(isVisible).values
    }

    def scenicScores: Grid[ScenicScore] = {
      def scenicScore(p: Point, h: TreeHeight) = {
        val top    = topView(p).takeUntil(_ < h).size
        val left   = leftView(p).takeUntil(_ < h).size
        val bottom = bottomView(p).takeUntil(_ < h).size
        val right  = rightView(p).takeUntil(_ < h).size

        (p -> top * left * bottom * right)
      }

      trees.map(scenicScore)
    }

    private def topView(p: Point)    = column(p.col).take(p.row).reverse
    private def bottomView(p: Point) = column(p.col).drop(p.row + 1)
    private def leftView(p: Point)   = row(p.row).take(p.col).reverse
    private def rightView(p: Point)  = row(p.row).drop(p.col + 1)

    private def row(row: Int): List[TreeHeight] =
      trees.rows.getOrElse(row, Map.empty).to(List).sortBy(_._1).map(_._2)

    private def column(col: Int): List[TreeHeight] =
      trees.columns.getOrElse(col, Map.empty).to(List).sortBy(_._1).map(_._2)

  }

  object Forest {

    def parse(lines: LazyList[String]): Forest = {
      val grid = Grid.fromRows(lines.map(_.map(_.asDigit)))
      Forest(grid)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val forest = Forest.parse(lines)

    val part1 = forest.visibleTrees.size
    val part2 = forest.scenicScores.values.max

    Result(part1.toString, part2.toString)
  }

}
