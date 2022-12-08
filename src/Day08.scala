package dev.agjacome.aoc2022

final case class Grid(rows: Seq[Seq[Int]]) {

  val columns: Seq[Seq[Int]] = rows.transpose

  val trees: Seq[Tree] =
    (0 to rows.size - 1).flatMap { i =>
      (0 to columns.size - 1).map { j =>
        Tree(
          height     = rows(i)(j),
          topView    = topOf(i, j),
          leftView   = leftOf(i, j),
          bottomView = bottomOf(i, j),
          rightView  = rightOf(i, j)
        )
      }
    }

  private def row(i: Int): Seq[Int]    = rows.lift(i).getOrElse(Seq.empty)
  private def column(j: Int): Seq[Int] = columns.lift(j).getOrElse(Seq.empty)

  private def topOf(i: Int, j: Int): Seq[Int]    = column(j).take(i).reverse
  private def leftOf(i: Int, j: Int): Seq[Int]   = row(i).take(j).reverse
  private def bottomOf(i: Int, j: Int): Seq[Int] = column(j).drop(i + 1)
  private def rightOf(i: Int, j: Int): Seq[Int]  = row(i).drop(j + 1)

}

object Grid {

  def parse(lines: Iterator[String]): Grid =
    Grid(rows = lines.to(Vector).map(_.to(Vector).map(_.asDigit)))

}

final case class Tree(
  height: Int,
  topView: Seq[Int],
  leftView: Seq[Int],
  bottomView: Seq[Int],
  rightView: Seq[Int]
) {

  import Tree._

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

  final implicit class SeqOps[A](private val self: Seq[A]) extends AnyVal {
    def takeUntil(f: A => Boolean): Seq[A] =
      self.span(f) match {
        case (head, tail) => head ++ tail.take(1)
      }
  }

}

object Day08 extends Day {

  def run(lines: Iterator[String]): Result = {
    val grid = Grid.parse(lines)

    val part1 = grid.trees.count(_.isVisible)
    val part2 = grid.trees.map(_.scenicScore).max

    Result(part1.toString, part2.toString)
  }

}
