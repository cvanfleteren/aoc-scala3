package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*



object Day4 extends App {

  case class Game(id: Int, winning:Set[Int], have: Set[Int]) {
    def intersecting: Set[Int] = this.winning.intersect(this.have)
  }

  def parseGame(in: String) : Game = {
    in.replace("Card ","").split(':').toList match {
      case game :: numbers :: Nil =>
        numbers.trim.split('|').toList match {
          case winning :: have :: Nil =>
            val winningNumbers = winning.trim.replace("  ", " ").split(' ').map(_.toInt).toSet
            val haveNumbers = have.trim.replace("  "," ").split(' ').map(_.toInt).toSet
            Game(game.trim.toInt, winningNumbers, haveNumbers)
        }
    }
  }

  def part1(input: String): Int = {
    val games = stringToLines(input).map(parseGame)

    def score(game: Game): Int = {
      val winningCount = game.intersecting.size
      if(winningCount >= 1) {
        val score = (2 to winningCount).foldLeft(1) { (score, number) =>
          score * 2
        }
        score
      } else {
        0
      }

    }

    games.map(score).sum
  }

  def part2(input: String): Int = {
    val games = stringToLines(input).map(parseGame)

    val initialCopyCountMap = games.map(g => g.id -> 1).toMap

    val copyCountMap = games.foldLeft(initialCopyCountMap) { (copyCountMap, game) =>
      val winningCount = game.intersecting.size
      if(winningCount > 0) {
        val currentGameCopycount = copyCountMap.getOrElse(game.id, 1)

        (game.id + 1).to(game.id + winningCount).foldLeft(copyCountMap) { (count, id) =>
          // add 1 copy to each of the games
          val currentCardCount = count.getOrElse(id, 1)
          val newCount  = count + (id -> (currentCardCount + currentGameCopycount))
          newCount
        } // but each copy of the current card also wins, so

      } else {
        copyCountMap
      }
    }
    println(copyCountMap)
    copyCountMap.values.sum
  }

  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
      |""".stripMargin

  def input = ParseUtils.inputForDay(4)

}
