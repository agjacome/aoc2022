package dev.agjacome.aoc2022

object Day10 extends Day {

  sealed abstract class Instruction(val cycles: Int)

  object Instruction {

    final case class Add(amount: Int) extends Instruction(2)
    final case object NoOp            extends Instruction(1)

    private val AddLine  = """^addx (-?\d+)$""".r
    private val NoOpLine = """^noop$""".r

    val parse: String => Instruction = {
      case AddLine(amount) => Add(amount.toInt)
      case NoOpLine()      => NoOp
      case line            => sys.error(s"Could not parse Instruction: ${line}")
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

  def run(lines: LazyList[String]): Result = {
    val cpuLog = lines
      .map(Instruction.parse)
      .foldLeft(List(CpuState.initial)) {
        case (states @ head :: _, instruction) => head.execute(instruction) ::: states
        case (Nil, instruction)                => CpuState.initial.execute(instruction)
      }
      .reverse

    val part1 = cpuLog
      .filter(cpu => Set(20, 60, 100, 140, 180, 220).contains(cpu.cycle))
      .map(_.signalStrength)
      .sum

    val part2 = cpuLog
      .foldLeft(CrtState.blank(40, 6))(_ drawPixel _)
      .print

    Result(part1.toString, part2)
  }

}
