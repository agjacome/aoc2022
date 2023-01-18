package dev.agjacome.aoc2022

import dev.agjacome.aoc2022.util.Grid
import dev.agjacome.aoc2022.util.Indexed
import dev.agjacome.aoc2022.util.Point
import dev.agjacome.aoc2022.util.ops._

object Day22 extends Day {

  sealed abstract class Direction(val value: Int) {
    def prev: Direction
    def next: Direction
  }

  object Direction {
    case object Up    extends Direction(3) { def prev = Left; def next = Right }
    case object Right extends Direction(0) { def prev = Up; def next = Down    }
    case object Down  extends Direction(1) { def prev = Right; def next = Left }
    case object Left  extends Direction(2) { def prev = Down; def next = Up    }
  }

  sealed abstract class Tile(val isWalkable: Boolean)

  object Tile {
    case object Open extends Tile(isWalkable = true)
    case object Wall extends Tile(isWalkable = false)
  }

  final case class Board(grid: Grid[Tile]) extends AnyVal {

    import Direction._

    def beginning: Point =
      grid.filter((_, tile) => tile.isWalkable).keys.min

    def next(point: Point, direction: Direction): Point = {
      val nextPoint = direction match {
        // direct movement
        case Up    if grid.contains(point.up)    => point.up
        case Right if grid.contains(point.right) => point.right
        case Down  if grid.contains(point.down)  => point.down
        case Left  if grid.contains(point.left)  => point.left

        // wrap-around edges
        case Up    => Point(row = grid.columns(point.col).keys.max, col = point.col)
        case Right => Point(row = point.row, col = grid.rows(point.row).keys.min)
        case Down  => Point(row = grid.columns(point.col).keys.min, col = point.col)
        case Left  => Point(row = point.row, col = grid.rows(point.row).keys.max)
      }

      grid(nextPoint).filter(_.isWalkable).fold(point)(_ => nextPoint)
    }

  }

  object Board {

    def parse(lines: LazyList[String]): Board = {
      val grid = lines.indexed
        .flatMap { case Indexed(columns, row) =>
          columns.to(Seq).indexed.collect {
            case Indexed('.', col) => Point(row, col) -> Tile.Open
            case Indexed('#', col) => Point(row, col) -> Tile.Wall
          }
        }
        .to(Map)

      Board(Grid(grid))
    }

  }

  sealed trait Step

  object Step {

    case object MoveForward extends Step
    case object TurnLeft    extends Step
    case object TurnRight   extends Step

    def parse(lines: LazyList[String]): List[Step] = {
      @scala.annotation.tailrec
      def loop(line: List[Char], acc: Vector[Step]): Vector[Step] = {
        val (number, rest) = line.span(_.isDigit)

        val forward = Vector.fill(number.mkString.toInt)(MoveForward)

        rest.to(List) match {
          case Nil         => acc ++ forward
          case 'L' :: tail => loop(tail, acc ++ forward :+ TurnLeft)
          case 'R' :: tail => loop(tail, acc ++ forward :+ TurnRight)
          case chr :: _    => sys.error(s"Unknown step '${chr}")
        }
      }

      lines.filter(_.nonEmpty).flatMap(line => loop(line.to(List), Vector.empty)).to(List)
    }

  }

  final case class Player(position: Point, direction: Direction) {

    import Step._

    def password: Int =
      1000 * (position.row + 1) + 4 * (position.col + 1) + direction.value

    @scala.annotation.tailrec
    def walk(steps: List[Step], board: Board): Player =
      steps match {
        case MoveForward :: tail =>
          this.copy(position = board.next(position, direction)).walk(tail, board)
        case TurnLeft :: tail =>
          this.copy(direction = direction.prev).walk(tail, board)
        case TurnRight :: tail =>
          this.copy(direction = direction.next).walk(tail, board)
        case Nil =>
          this
      }

  }

  def run(lines: LazyList[String]): Result = {
    val (boardLines, stepLines) = lines.span(_.nonEmpty)

    val board = Board.parse(boardLines)
    val steps = Step.parse(stepLines)

    val part1 = Player(board.beginning, Direction.Right).walk(steps, board).password

    Result(part1.toString, "")
  }

}
