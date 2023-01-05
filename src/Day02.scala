package dev.agjacome.aoc2022

object Day02 extends Day {

  sealed abstract class Shape(val score: Int) {

    def beats: Shape

    def playAgainst(that: Shape): Outcome =
      if (this == that)
        Outcome.Draw
      else if (this.beats == that)
        Outcome.Win
      else
        Outcome.Loss

  }

  object Shape {

    case object Rock     extends Shape(score = 1) { val beats = Scissors }
    case object Paper    extends Shape(score = 2) { val beats = Rock     }
    case object Scissors extends Shape(score = 3) { val beats = Paper    }

    val fromOpponentSymbol: Char => Shape = {
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors
      case sym => sys.error(s"Unknown Shape symbol ${sym}")
    }

    val fromPlayerSymbol: Char => Shape = {
      case 'X' => Rock
      case 'Y' => Paper
      case 'Z' => Scissors
      case sym => sys.error(s"Unknown Shape symbol ${sym}")
    }

  }

  sealed abstract class Outcome(val score: Int) {

    def guessPlayer(opponent: Shape): Shape =
      this match {
        case Outcome.Draw => opponent
        case Outcome.Loss => opponent.beats
        case Outcome.Win  => opponent.beats.beats
      }

  }

  object Outcome {

    case object Draw extends Outcome(3)
    case object Loss extends Outcome(0)
    case object Win  extends Outcome(6)

    val fromPlayerSymbol: Char => Outcome = {
      case 'X' => Loss
      case 'Y' => Draw
      case 'Z' => Win
      case sym => sys.error(s"Unknown Outcome symbol ${sym}")
    }

  }

  def run(lines: LazyList[String]): Result = {
    val symbols = lines
      .map(_.split(' '))
      .collect { case Array(opponent, player) => (opponent.head, player.head) }

    val part1 = symbols.map { case (opponentSymbol, playerSymbol) =>
      val opponent = Shape.fromOpponentSymbol(opponentSymbol)
      val player   = Shape.fromPlayerSymbol(playerSymbol)
      val outcome  = player.playAgainst(opponent)

      outcome.score + player.score
    }.sum

    val part2 = symbols.map { case (opponentSymbol, playerSymbol) =>
      val opponent = Shape.fromOpponentSymbol(opponentSymbol)
      val outcome  = Outcome.fromPlayerSymbol(playerSymbol)
      val player   = outcome.guessPlayer(opponent)

      outcome.score + player.score
    }.sum

    Result(part1.toString, part2.toString)
  }

}
