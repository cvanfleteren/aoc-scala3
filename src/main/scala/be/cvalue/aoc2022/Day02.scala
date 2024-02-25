import Day02.Choice.*
import Day02.Result.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object Day02 extends App {

  val testInput =
    s"""A Y
       |B X
       |C Z
       |""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day02.txt"))

  enum Choice(val score: Int) {
    case Rock extends Choice(1)
    case Paper extends Choice(2)
    case Scissors extends Choice(3)
  }

  enum Result(val score: Int) {
    case Win extends Result(6)
    case Loss extends Result(0)
    case Tie extends Result(3)
  }

  def parseLine(line: String): (Choice, Choice) = {
    val parts = line.split(' ').map {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
    }
    (parts.head, parts.last)
  }

  def calcScore(choices: (Choice, Choice)): Int = {
    val WIN = 6
    val TIE = 3
    val LOSS = 0

    val winOrLoss = choices match {
      case (Rock, Paper) => WIN
      case (Rock, Scissors) => LOSS

      case (Paper, Rock) => LOSS
      case (Paper, Scissors) => WIN

      case (Scissors, Rock) => WIN
      case (Scissors, Paper) => LOSS

      case (_, _) => TIE
    }

    winOrLoss + choices._2.score
  }


  def parseLine2(lijn: String): (Choice, Result) = {
    val parts = lijn.split(' ')
    val opponent = parts(0) match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

    val desiredOutcome = parts(1) match {
      case "X" => Loss
      case "Y" => Tie
      case "Z" => Win
    }
    (opponent, desiredOutcome)
  }

  def calcScore2(input: (Choice, Result)): Int = {

    val choiceScore = input match {
      case (o, Tie) => o.score
      case (Rock, Win) => Paper.score
      case (Rock, Loss) => Scissors.score
      case (Paper, Win) => Scissors.score
      case (Paper, Loss) => Rock.score
      case (Scissors, Win) => Rock.score
      case (Scissors, Loss) => Paper.score
    }

    choiceScore + input._2.score
  }



  def part1(input: String): Int = {
    val scores = input.split('\n').toList.map(lijn => parseLine(lijn)).map(calcScore)
    scores.sum
  }


  def part2(input: String): Int = {
    val scores = input.split('\n').toList.map(parseLine2).map(calcScore2)
    scores.sum
  }

  println(part1(input))
  println(part2(input))
}