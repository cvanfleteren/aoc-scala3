import Day02.Choice.*
import Day02.Result.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object Day03 extends App {

  val testInput =
    s"""vJrwpWtwJgWrhcsFMMfFFhFp
       |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
       |PmmdzqPrVvPwwTWBwg
       |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
       |ttgJtRGJQctTZtZT
       |CrZsJsPPZsGzwwsLwLmpwMDw
       |""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day03.txt"))


  def parseLine(in: String): (String, String) = {
    in.splitAt(in.length / 2)
  }

  def part1(in: String): Int = {
    val commons = in.split('\n').map(parseLine).map { case (p1, p2) =>
      p1.toSet.intersect(p2.toSet)
    }
    totalScore(commons)
  }

  def totalScore(commons:IterableOnce[Set[Char]]): Int = {
    commons.map { common =>
      common.map(scoreLetter).sum
    }.sum
  }

  def scoreLetter(c: Char): Int = {
    if (c.isLower) {
      c - 96
    } else {
      c - 64 + 26
    }
  }

  def part2(in: String): Int = {
    val commons = in.split('\n').map(_.toSet).grouped(3).map { group =>
      val common = group.fold(group.head) { case (a, b) =>
        a.intersect(b)
      }
      common
    }
    totalScore(commons)
  }

  println(part1(input))
  println(part2(input))
}