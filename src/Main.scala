package dev.agjacome.aoc2022

import scala.io.Source
import scala.util.Try

final class Main(args: Main.Arguments) {

  private type ErrorOr[A] = Either[String, A]

  def run(): ErrorOr[Result] =
    for {
      day    <- getDay(args.dayNumber)
      source <- getSource(args.inputFile)
    } yield day.run(source.getLines())

  private def getDay(dayArg: Option[String]): ErrorOr[Day] =
    for { 
      dayStr <- dayArg.toRight("USAGE: ./Main day_number [input_file]")
      dayNum <- dayStr.toIntOption.toRight(s"'${dayStr}' is not a valid Day number")
      day    <- Day.get(dayNum).toRight(s"Day ${dayNum} not found")
    } yield day

  private def getSource(fileArg: Option[String]): ErrorOr[Source] = {
    val source = Try {
      fileArg.fold(Source.stdin)(Source.fromFile)
    }

    source.toEither.left.map(err => s"Could not load input: ${err.getMessage}")
  }

}

object Main {

  final case class Arguments(
    dayNumber: Option[String],
    inputFile: Option[String]
  )

  object Arguments {

    def from(args: Array[String]): Arguments =
      Arguments(
        dayNumber = args.lift(0),
        inputFile = args.lift(1)
      )

  }

  def main(args: Array[String]): Unit = {
    val main = new Main(Arguments.from(args))

    main.run() match {
      case Right(Result(part1, part2)) =>
        Console.out.println(s"Part 1: ${part1}")
        Console.out.println(s"Part 2: ${part2}")

      case Left(err) =>
        Console.err.println(err)
    }
  }

}
