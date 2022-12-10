package dev.agjacome.aoc2022

sealed abstract class Instruction(val cycles: Int)

object Instruction {

  final case class Add(amount: Int) extends Instruction(2)
  final case object NoOp            extends Instruction(1)

  def parse(line: String): Option[Instruction] = {
    val AddLine  = """^addx (-?\d+)$""".r
    val NoOpLine = """^noop$""".r

    line match {
      case AddLine(amount) => Some(Add(amount.toInt))
      case NoOpLine()      => Some(NoOp)
      case _               => None
    }
  }

}

final case class CpuState(register: Int, cycle: Int) {

  import Instruction._

  def signalStrength: Int = register * cycle

  def execute(instruction: Instruction): List[CpuState] = {
    val executing =
      List.tabulate(instruction.cycles - 1)(c => this.copy(cycle = cycle + c + 1)).reverse

    instruction match {
      case Add(amount) => CpuState(register + amount, cycle + instruction.cycles) :: executing
      case NoOp        => CpuState(register, cycle + instruction.cycles) :: executing
    }
  }

}

object CpuState {

  val initial = CpuState(register = 1, cycle = 1)

}

final case class CrtState(width: Int, height: Int, pixels: Vector[Char]) {

  import CrtState._

  def drawPixel(cpu: CpuState): CrtState = {
    val pixel = cpu.cycle - 1

    if (pixel < 0 || pixel >= pixels.size) {
      this
    } else {
      val value = if (((pixel % width) - cpu.register).abs < 2) litPixel else darkPixel
      this.copy(pixels = pixels.updated(pixel, value))
    }
  }

  def print: String =
    pixels
      .grouped(width)
      .take(height)
      .map(_.mkString)
      .mkString(start = "\n", sep = "\n", end = "")

}

object CrtState {

  val litPixel  = '#'
  val darkPixel = '.'

  def blank(width: Int, height: Int): CrtState =
    CrtState(width, height, Vector.fill(width * height)(darkPixel))

}

object Day10 extends Day {

  def run(lines: Iterator[String]): Result = {
    val instructions = lines.flatMap(Instruction.parse)

    val cpuStateLog = instructions
      .foldLeft(List(CpuState.initial)) {
        case (states @ head :: _, instruction) => head.execute(instruction) ::: states
        case (Nil, instruction)                => CpuState.initial.execute(instruction)
      }
      .reverse
      .to(LazyList)

    val part1 = cpuStateLog
      .filter(cpu => Set(20, 60, 100, 140, 180, 220).contains(cpu.cycle))
      .map(_.signalStrength)
      .sum

    val part2 = cpuStateLog
      .foldLeft(CrtState.blank(40, 6))(_ drawPixel _)
      .print

    Result(part1.toString, part2)
  }

}
