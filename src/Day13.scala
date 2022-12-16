package dev.agjacome.aoc2022

import scala.math.Ordering.Implicits._

import dev.agjacome.aoc2022.util.Tree
import dev.agjacome.aoc2022.util.Tree.{Leaf, Node}

object Day13 extends Day {

  type Packet = Tree[Int]

  implicit val order: Ordering[Packet] = {
    case (Leaf(left), Leaf(right))         => Ordering[Int].compare(left, right)
    case (Node(left), Node(right))         => Ordering[Vector[Packet]].compare(left, right)
    case (left @ Leaf(_), right @ Node(_)) => order.compare(Node(Vector(left)), right)
    case (left @ Node(_), right @ Leaf(_)) => order.compare(left, Node(Vector(right)))
  }

  def Packet(line: String): Packet = {
    def loop0(chars: List[Char], acc: Vector[Packet]): Vector[Packet] =
      chars match {
        case Nil =>
          acc

        case '[' :: tail =>
          val (balanced, rest) = loop1('[' :: tail, 0, List.empty)
          loop0(rest, acc :+ Node(loop0(balanced, Vector.empty)))

        case num :: _ if num.isDigit =>
          val (digits, rest) = chars.span(_.isDigit)
          loop0(rest, acc :+ Leaf(digits.mkString.toInt))

        case _ :: tail =>
          loop0(tail, acc)
      }

    @scala.annotation.tailrec
    def loop1(chars: List[Char], open: Int, acc: List[Char]): (List[Char], List[Char]) = {
      (chars, open) match {
        case (Nil, 0) => (acc.reverse, Nil)
        case (Nil, _) => (Nil, chars)

        case ('[' :: tail, 0) => loop1(tail, open + 1, acc)
        case ('[' :: tail, _) => loop1(tail, open + 1, '[' :: acc)

        case (']' :: _, 0)    => (Nil, chars)
        case (']' :: tail, 1) => (acc.reverse, tail)
        case (']' :: tail, _) => loop1(tail, open - 1, ']' :: acc)

        case (chr :: tail, _) => loop1(tail, open, chr :: acc)
      }
    }

    val (balanced, _) = loop1(line.to(List), 0, List.empty)
    Node(loop0(balanced, Vector.empty))
  }

  def run(lines: LazyList[String]): Result = {
    val packets = lines.filterNot(_.isEmpty).map(Packet)

    val part1 = {
      val grouped = packets.grouped(2).zipWithIndex
      val indices = grouped.collect {
        case (Seq(left, right), idx) if left <= right => idx + 1
      }

      indices.sum
    }

    val part2 = {
      val divider1 = Packet("[[2]]")
      val divider2 = Packet("[[6]]")

      val sorted = (divider1 #:: divider2 #:: packets).sorted
      (sorted.indexOf(divider1) + 1) * (sorted.indexOf(divider2) + 1)
    }

    Result(part1.toString, part2.toString)
  }

}
