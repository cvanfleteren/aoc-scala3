package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils._

import scala.collection.parallel.CollectionConverters.*

object Day9 extends App {

  type Input = List[List[Long]]


  def parse(in: String): Input = {
    ParseUtils.stringToLines(in).map(lineToNumbers())
  }

  def part1(in: String): Long = {
    val input = parse(in)
    input
      .map(allDifferences)
      .map(extrapolateNext)
      .map(_.head).sum
  }

  def part2(in: String): Long = {
    val input = parse(in)
    input
      .map(allDifferences)
      .map(extrapolatePrevious)
      .map(_.head).sum
  }


  def extrapolateNext(in: List[List[Long]]): List[Long] = {
    val lastOfEach = in.reverse.map(_.last)
    lastOfEach.tail.foldLeft(List(0L)) { (sum, current) =>
      sum.head + current :: sum
    }
  }

  def extrapolatePrevious(in: List[List[Long]]): List[Long] = {
    val firstOfEach = in.reverse.map(_.head)
    firstOfEach.tail.foldLeft(List(0L)) { (sum, current) =>
      current - sum.head :: sum
    }
  }

  def allDifferences(in: List[Long]): List[List[Long]] = {

    def differences(in: List[Long]): List[Long] = {
      val r = in.sliding(2).map { two => two.last - two.head }
      r.toList
    }

    def diffAccum(in: List[Long], accum: List[List[Long]]): List[List[Long]] = {
      if (in.exists(_ != 0)) {
        val diff = differences(in)
        diffAccum(diff, diff :: accum)
      } else {
        accum
      }
    }

    val accum: List[List[Long]] = Nil
    in :: diffAccum(in, accum).reverse
  }

  println(extrapolatePrevious(allDifferences(List(10,  13,  16,  21,  30,  45))))

  println(part1(input))
  println(part2(input))


  lazy val input2 =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  def input = ParseUtils.inputForDay(9)

}
