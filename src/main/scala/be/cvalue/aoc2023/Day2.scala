package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

object Day2 extends App {

  lazy val input2 =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  def input = ParseUtils.inputForDay(2)

  case class Game(id: Int, rounds: List[Colors])

  case class Colors(r: Int = 0, g: Int = 0, b: Int = 0)

  def parseGame(line: String): Game = {
    def parseId(in: String): Int = {
      in.split(' ').last.toInt
    }

    def parseColors(in: String): Colors = {
      in.split(',').toList.foldLeft(Colors()) { case (colors, colorPart) =>
        colorPart.trim.split(' ').toList match {
          case count :: "red" :: Nil => colors.copy(r = count.toInt)
          case count :: "green" :: Nil => colors.copy(g = count.toInt)
          case count :: "blue" :: Nil => colors.copy(b = count.toInt)
        }
      }
    }

    def parseRounds(in: String): List[Colors] = {
      in.split(';').toList.map(parseColors)
    }

    line.split(':').toList match {
      case id :: rounds :: Nil => Game(id = parseId(id), parseRounds(rounds))
    }
  }

  def part1(input: String): Int = {
    def isPossible(game: Game): Boolean = {
      !game.rounds.exists{ round =>
        round.r > 12 || round.g > 13 || round.b > 14
      }
    }

    val games = ParseUtils.stringToLines(input).map(parseGame)
    games.filter(isPossible).map(_.id).sum
  }

  def part2(input: String): Int = {
    def calcPower(game: Game): Int = {
      val r = game.rounds.map(_.r).max
      val g = game.rounds.map(_.g).max
      val b = game.rounds.map(_.b).max
      r * g * b
    }

    val games = ParseUtils.stringToLines(input).map(parseGame)
    games.map(calcPower).sum
  }

  println(part1(input))
  println(part2(input))
}
