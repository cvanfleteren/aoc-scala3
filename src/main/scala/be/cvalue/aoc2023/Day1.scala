package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

object Day1 extends App {

  def part1(input: String): Int = {
    stringToLines(input).map { line =>
      val digits = line.filter(_.isDigit)
      List(digits.head, digits.last).mkString.toInt
    }.sum
  }

  def part2(input: String): Int = {
    // walk through line character by character, if it's a digit, or starts with a written number
    // representing a digit, add that digit to the list
    stringToLines(input).map { line =>
      case class State(line: String, digits: List[Int])

      val res = line.foldLeft(State(line, Nil)) { case (state, char) =>

        val number = if (state.line.startsWith("one")) {
          1
        } else if (state.line.startsWith("two")) {
          2
        } else if (state.line.startsWith("three")) {
          3
        } else if (state.line.startsWith("four")) {
          4
        } else if (state.line.startsWith("five")) {
          5
        } else if (state.line.startsWith("six")) {
          6
        } else if (state.line.startsWith("seven")) {
          7
        } else if (state.line.startsWith("eight")) {
          8
        } else if (state.line.startsWith("nine")) {
          9
        } else if (state.line.head.isDigit) {
          state.line.head.toString.toInt
        } else {
          0
        }

        if (number > 0) {
          state.copy(line = state.line.tail, digits = number :: state.digits)
        } else {
          state.copy(line = state.line.tail)
        }
      }
      //reverse order!
      List(res.digits.last, res.digits.head).mkString.toInt
    }.sum
  }

  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  def input = ParseUtils.inputForDay(1)

}
