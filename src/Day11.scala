package dev.agjacome.aoc2022

import scala.collection.immutable.Queue

import dev.agjacome.aoc2022.util.ops._

object Day11 extends Day {

  sealed abstract class WorryLevel(level: Long) {
    def apply(itemLevel: Long): Long = itemLevel / this.level
  }

  object WorryLevel {
    case object Relaxed extends WorryLevel(3)
    case object Anxious extends WorryLevel(1)
  }

  final case class Monkey(
      id: Monkey.Id,
      items: Queue[Monkey.Item],
      operation: Monkey.Item => Monkey.Item,
      divisor: Long,
      throwItem: Boolean => Monkey.Id,
      itemsInspected: Long
  ) {

    def receive(item: Monkey.Item): Monkey =
      this.copy(items = items :+ item)

    def playTurn(
        worryLevel: WorryLevel,
        normalizeItem: Monkey.Item => Monkey.Item
    ): (Monkey, Queue[(Monkey.Id, Monkey.Item)]) = {
      val updateItem = operation.andThen(normalizeItem).andThen(worryLevel.apply)

      val updatedMonkey = this.copy(
        items = Queue.empty,
        itemsInspected = itemsInspected + items.size
      )

      val throws = items.map { item =>
        val itemLevel   = updateItem(item)
        val condition   = itemLevel % divisor == 0
        val destination = throwItem(condition)

        destination -> itemLevel
      }

      (updatedMonkey, throws)
    }

  }

  object Monkey {

    type Id   = Int
    type Item = Long

    private val MonkeyLines =
      """|(?s)^Monkey (\d+):
       |\s{2}Starting items: (\d+(?:, \d+)*)+
       |\s{2}Operation: new = (old|\d+) ([+*]) (old|\d+)
       |\s{2}Test: divisible by (\d+)
       |\s{4}If true: throw to monkey (\d+)
       |\s{4}If false: throw to monkey (\d+)$""".stripMargin.r

    val parse: String => Monkey = {
      case MonkeyLines(id, items, operandL, operator, operandR, divisor, truthy, falsey) =>
        def parsedId    = id.toInt
        val parsedItems = items.split(", ").map(_.toLong).to(Queue)

        val operand = (item: Item, operand: String) =>
          if (operand == "old") item else operand.toLong

        val parsedOperation: Item => Item = operator match {
          case "+" => item => operand(item, operandL) + operand(item, operandR)
          case "*" => item => operand(item, operandL) * operand(item, operandR)
          case _   => identity
        }

        val parsedDivisor = divisor.toLong
        val parsedThrows  = if (_) truthy.toInt else falsey.toInt

        Monkey(
          id = parsedId,
          items = parsedItems,
          operation = parsedOperation,
          divisor = parsedDivisor,
          throwItem = parsedThrows,
          itemsInspected = 0
        )

      case lines =>
        sys.error(s"Could not parse Monkey:\n${lines}")
    }

    def parse(lines: LazyList[String]): LazyList[Monkey] =
      lines
        .filterNot(_.isEmpty)
        .grouped(6)
        .map(_.mkString("\n"))
        .map(parse)
        .to(LazyList)

  }

  final case class KeepAwayGame(monkeys: Map[Monkey.Id, Monkey], lcm: Long) {

    private def normalizeItem(item: Monkey.Item): Monkey.Item =
      item % lcm

    @scala.annotation.tailrec
    def playRounds(rounds: Int, worry: WorryLevel): KeepAwayGame =
      if (rounds <= 0)
        this
      else
        this.playRound(worry).playRounds(rounds - 1, worry)

    def playRound(worry: WorryLevel): KeepAwayGame = {
      @scala.annotation.tailrec
      def loop(
          ids: List[Monkey.Id],
          acc: Map[Monkey.Id, Monkey]
      ): Map[Monkey.Id, Monkey] =
        ids match {
          case monkeyId :: next =>
            val (updated, throws) = acc(monkeyId).playTurn(worry, normalizeItem)

            val updatedAcc0 = acc.updated(monkeyId, updated)
            val updatedAcc1 = throws.foldLeft(updatedAcc0) { case (acc, (id, items)) =>
              acc.updating(id)(_.receive(items))
            }

            loop(next, updatedAcc1)

          case Nil => acc
        }

      val monkeyIds = monkeys.keys.to(List).sorted
      this.copy(monkeys = loop(monkeyIds, monkeys))
    }

    def monkeyBusiness: Long =
      monkeys.values
        .map(_.itemsInspected)
        .to(List)
        .sorted(Ordering[Long].reverse)
        .take(2)
        .product

  }

  object KeepAwayGame {

    def from(monkeys: Iterable[Monkey]): KeepAwayGame = {
      val lcm = {
        def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
        def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

        monkeys.map(_.divisor).foldLeft(1L)(lcm)
      }

      KeepAwayGame(monkeys.map(m => m.id -> m).to(Map), lcm)
    }

  }

  def run(lines: LazyList[String]): Result = {
    val monkeys = Monkey.parse(lines)
    val game    = KeepAwayGame.from(monkeys)

    val part1 = game.playRounds(20, WorryLevel.Relaxed).monkeyBusiness
    val part2 = game.playRounds(10000, WorryLevel.Anxious).monkeyBusiness

    Result(part1.toString, part2.toString)
  }

}
