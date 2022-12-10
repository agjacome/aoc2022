package dev.agjacome.aoc2022

import scala.collection.immutable.SortedMap

final case class CrateStack(stacks: SortedMap[Int, List[Char]]) extends AnyVal {

  def topmost: List[Char] =
    stacks.values.flatMap(_.headOption).toList

  def get(id: Int): List[Char] =
    stacks.getOrElse(id, List.empty)

  def +(kv: (Int, List[Char])): CrateStack =
    CrateStack(stacks + kv)

}

object CrateStack {

  def apply(crates: Seq[(Int, List[Char])]): CrateStack =
    CrateStack(SortedMap.from(crates))

  def parse(lines: Seq[String]): CrateStack =
    CrateStack(
      lines.toList.transpose
        .map(_.filter(_.isLetter))
        .filter(_.nonEmpty)
        .zipWithIndex
        .map { case (stack, id) => id + 1 -> stack }
    )

}

final case class Rearrangement(move: Int, from: Int, to: Int)

object Rearrangement {

  val MoveRegex = """move (\d+) from (\d+) to (\d+)""".r

  def parseAll(lines: Seq[String]): Seq[Rearrangement] =
    lines.collect { case MoveRegex(move, from, to) =>
      Rearrangement(move.toInt, from.toInt, to.toInt)
    }

}

final class CargoCrane private (mode: CargoCrane.Mode) {

  import CargoCrane._

  def rearrange(stack: CrateStack, r: Rearrangement): CrateStack = {
    val from = stack.get(r.from)
    val to   = stack.get(r.to)

    val (movingCrates, newFrom) = from.splitAt(r.move)

    val newTo = mode match {
      case Mode.Single   => (movingCrates.reverse) ::: to
      case Mode.Multiple => movingCrates ::: to
    }

    stack + (r.from -> newFrom) + (r.to -> newTo)
  }

}

object CargoCrane {

  sealed trait Mode

  object Mode {
    case object Single   extends Mode
    case object Multiple extends Mode
  }

  val single = new CargoCrane(Mode.Single)
  val multi  = new CargoCrane(Mode.Multiple)

}

object Day05 extends Day {

  def run(lines: LazyList[String]): Result = {
    val (startStacks, rearrangeProcedure) =
      lines.span(_.nonEmpty) match {
        case (s, p) => (CrateStack.parse(s), Rearrangement.parseAll(p))
      }

    val part1 = rearrangeProcedure.foldLeft(startStacks)(CargoCrane.single.rearrange).topmost
    val part2 = rearrangeProcedure.foldLeft(startStacks)(CargoCrane.multi.rearrange).topmost

    Result(part1.mkString, part2.mkString)
  }

}
