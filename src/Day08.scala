package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.predef._

object Day08 extends Day {

  final case class Tree(
      height: Int,
      topView: Seq[Int],
      leftView: Seq[Int],
      bottomView: Seq[Int],
      rightView: Seq[Int]
  ) {

    val isVisible: Boolean = {
      val top    = topView.forall(_ < height)
      val left   = leftView.forall(_ < height)
      val bottom = bottomView.forall(_ < height)
      val right  = rightView.forall(_ < height)

      top || left || bottom || right
    }

    val scenicScore: Int = {
      val top    = topView.takeUntil(_ < height).size
      val left   = leftView.takeUntil(_ < height).size
      val bottom = bottomView.takeUntil(_ < height).size
      val right  = rightView.takeUntil(_ < height).size

      top * left * bottom * right
    }

  }

  object Tree {

    def fromHeightGrid(grid: Grid[Int]): List[Tree] =
      grid.map { case (coord, cell) =>
        Tree(
          height = cell,
          topView = grid.topView(coord),
          leftView = grid.leftView(coord),
          bottomView = grid.bottomView(coord),
          rightView = grid.rightView(coord)
        )
      }

  }

  def run(lines: LazyList[String]): Result = {
    val rows = lines.map(_.map(_.asDigit))
    val grid = Grid.fromRows(rows)

    val trees = Tree.fromHeightGrid(grid)

    val part1 = trees.count(_.isVisible)
    val part2 = trees.map(_.scenicScore).max

    Result(part1.toString, part2.toString)
  }

}
