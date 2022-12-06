package dev.agjacome.aoc2022

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

object Day02 extends Day {

  def getGameScores(opponentSymbol: String, playerSymbol: String): Option[(Int, Int)] =
    for {
      opponent <- Shape.fromOpponentSymbol(opponentSymbol)

      // part 1 score:
      player1  <- Shape.fromPlayerSymbol(playerSymbol)
      outcome1  = player1.playAgainst(opponent)
      score1    = outcome1.score + player1.score

      // part 2 score:
      outcome2 <- Outcome.fromPlayerSymbol(playerSymbol)
      player2   = outcome2.guessPlayer(opponent)
      score2    = outcome2.score + player2.score
    } yield (score1, score2)

  def run(lines: Iterator[String]): Result =  {
    val result = lines
      .flatMap(_.split(' ') match {
        case Array(opponent, player) =>
          getGameScores(opponent, player)
      })
      .foldLeft((0, 0)) {
        case ((total1, total2), (partial1, partial2)) =>
          (total1 + partial1, total2 + partial2)
      }

    Result(result._1.toString, result._2.toString)
  }

}
