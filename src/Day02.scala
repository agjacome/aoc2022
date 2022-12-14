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

    val fromOpponentSymbol: String => Option[Shape] =
      Map("A" -> Rock, "B" -> Paper, "C" -> Scissors).get

    val fromPlayerSymbol: String => Option[Shape] =
      Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors).get

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

    val fromPlayerSymbol: String => Option[Outcome] =
      Map("X" -> Loss, "Y" -> Draw, "Z" -> Win).get

  }

  def run(lines: LazyList[String]): Result = {
    val symbols = lines
      .map(_.split(' '))
      .collect { case Array(opponent, player) => (opponent, player) }

    val part1 = symbols.flatMap { case (opponentSymbol, playerSymbol) =>
      for {
        opponent <- Shape.fromOpponentSymbol(opponentSymbol)
        player   <- Shape.fromPlayerSymbol(playerSymbol)

        outcome = player.playAgainst(opponent)
        score   = outcome.score + player.score
      } yield score
    }.sum

    val part2 = symbols.flatMap { case (opponentSymbol, playerSymbol) =>
      for {
        opponent <- Shape.fromOpponentSymbol(opponentSymbol)
        outcome  <- Outcome.fromPlayerSymbol(playerSymbol)

        player = outcome.guessPlayer(opponent)
        score  = outcome.score + player.score
      } yield score
    }.sum

    Result(part1.toString, part2.toString)
  }

}
