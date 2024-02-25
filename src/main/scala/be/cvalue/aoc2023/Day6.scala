package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

import scala.collection.parallel.CollectionConverters._

object Day6 extends App {

  def calcRaceWinPossibilities(time: Int, currentRecord: Long): Int = {
    0.to(time).par.count { timeButtonHeld =>
      val speed = timeButtonHeld
      val distanceCovered = speed.toLong * (time - timeButtonHeld)
      distanceCovered > currentRecord
    }
  }

  def part1(input: String): Long = {

    def parse(in: String): List[(Int, Int)] = {
      val lines = ParseUtils.stringToLines(in)
      val times = lines.head.filter(c => c.isDigit || c == ' ').mkString.trim.split(' ').filterNot(_.trim.isEmpty).map(_.trim.toInt)
      val distances = lines.last.filter(c => c.isDigit || c == ' ').mkString.trim.split(' ').filterNot(_.trim.isEmpty).map(_.trim.toInt)

      times.toList.zip(distances)
    }

    parse(input).map((time, currentRecord) => calcRaceWinPossibilities(time, currentRecord)).product
  }

  def part2(input: String): Long = {
    def parse(in: String): (Long, Long) = {
      val lines = ParseUtils.stringToLines(in)
      val time = lines.head.filter(c => c.isDigit).mkString.toLong
      val distance = lines.last.filter(c => c.isDigit).mkString.toLong

      (time, distance)
    }

    val (time, distance) = parse(input)
    calcRaceWinPossibilities(time.toInt, distance)
  }

  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  def input = ParseUtils.inputForDay(6)

}
