package dev.agjacome.aoc2022

import scala.collection.immutable.Queue

final case class Monkey(
    id: Monkey.Id,
    items: Queue[Monkey.Item],
    operation: Monkey.Item => Monkey.Item,
    divisor: Long,
    throwItem: Boolean => Monkey.Id,
    inspectCount: Long = 0
) {

  def receive(item: Monkey.Item): Monkey =
    this.copy(items = items :+ item)

  def play(lcm: Long, worryLevelDivisor: Long): (Monkey, Queue[(Monkey.Id, Monkey.Item)]) = {
    val updated = this.copy(items = Queue.empty, inspectCount = inspectCount + items.size)

    val throws = items.map { item =>
      val itemLevel   = (operation(item) % lcm) / worryLevelDivisor
      val condition   = itemLevel % divisor == 0
      val destination = throwItem(condition)

      destination -> itemLevel
    }

    (updated, throws)
  }

}

object Monkey {

  type Id   = Int
  type Item = Long

  implicit val ordering: Ordering[Monkey] = Ordering.by(_.id)

  private val MonkeyLines =
    """|(?s)^Monkey (\d+):
       |\s{2}Starting items: (\d+(?:, \d+)*)+
       |\s{2}Operation: new = (old|\d+) ([+*]) (old|\d+)
       |\s{2}Test: divisible by (\d+)
       |\s{4}If true: throw to monkey (\d+)
       |\s{4}If false: throw to monkey (\d+)$""".stripMargin.r

  private def buildMonkey(
      id: String,
      items: String,
      operandX: String,
      operator: String,
      operandY: String,
      divisor: String,
      truthy: String,
      falsey: String
  ): Monkey = {
    val parsedId    = id.toInt
    val parsedItems = items.split(", ").map(_.toLong).to(Queue)

    val parseOperand = (item: Item, operand: String) =>
      if (operand == "old") item else operand.toLong

    val parsedOperation: Item => Item = operator match {
      case "+" => (item: Item) => parseOperand(item, operandX) + parseOperand(item, operandY)
      case "*" => (item: Item) => parseOperand(item, operandX) * parseOperand(item, operandY)
      case _   => identity
    }

    val parsedDivisor = divisor.toLong
    val parsedThrow: Boolean => Id  = cond => if (cond) truthy.toInt else falsey.toInt

    Monkey(parsedId, parsedItems, parsedOperation, parsedDivisor, parsedThrow)

  }

  def parse: String => Option[Monkey] = {
    case MonkeyLines(id, items, opX, op, opY, test, truthy, falsey) =>
      Some(buildMonkey(id, items, opX, op, opY, test, truthy, falsey))
    case _ =>
      None
  }

  def parseAll(lines: Seq[String]): List[Monkey] =
    lines.filterNot(_.isEmpty).grouped(6).map(_.mkString("\n")).flatMap(parse).to(List)

}

final case class MonkeyGame(monkeys: List[Monkey], worryLevelDivisor: Long) {

  val lcm = monkeys.map(_.divisor).product

  def playRounds(n: Int): MonkeyGame = 
    if (n <= 0) this else this.playRound.playRounds(n - 1)

  def playRound: MonkeyGame = {
    @scala.annotation.tailrec
    def loop(id: Monkey.Id, acc: List[Monkey]): List[Monkey] =
      acc.lift(id) match {
        case None         => acc
        case Some(monkey) =>
          val (updated, throws) = monkey.play(lcm, worryLevelDivisor)

          val nextAcc = throws.foldLeft(acc.updated(monkey.id, updated)) {
            case (monkeys, (id, item)) =>
              monkeys.updated(id, monkeys(id).receive(item))
          }

          loop(id + 1, nextAcc)
      }

    this.copy(monkeys = loop(0, monkeys))
  }

  def monkeyBusiness: Long =
    monkeys.map(_.inspectCount).sorted(Ordering[Long].reverse).take(2).product

}

object Day11 extends Day {

  def run(lines: LazyList[String]): Result = {
    val monkeys = Monkey.parseAll(lines).to(List).sorted

    val part1 = MonkeyGame(monkeys, 3).playRounds(20).monkeyBusiness
    val part2 = MonkeyGame(monkeys, 1).playRounds(10000).monkeyBusiness

    Result(part1.toString, part2.toString)
  }

}
