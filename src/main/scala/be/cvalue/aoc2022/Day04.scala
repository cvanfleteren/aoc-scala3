import Day02.Choice.*
import Day02.Result.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object Day04 extends App {

  val testInput =
    s"""2-4,6-8
       |2-3,4-5
       |5-7,7-9
       |2-8,3-7
       |6-6,4-6
       |2-6,4-8""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day04.txt"))


  def parseLine(in: String): (Range, Range) = {
    val ranges = in.split(',').map { part =>
      val parts = part.split('-').map(_.toInt)
      Range.inclusive(parts.head, parts.last)
    }
    (ranges.head, ranges.last)
  }

  def part1(in: String): Int = {
    in.split('\n').map(parseLine).count(isFullyOverlapping)
  }

  def part2(in: String): Int = {
    in.split('\n').map(parseLine).count(overlaps)
  }

  def isFullyOverlapping(r: (Range, Range)): Boolean = {
    val (r1, r2) = r
    r1.head <= r2.head && r1.last >= r2.last ||
      r2.head <= r1.head && r2.last >= r1.last
  }

  def overlaps(r: (Range, Range)): Boolean = {
    val (r1, r2) = r
    r1.contains(r2.head) || r1.contains(r2.last) || r2.contains(r1.head) || r2.contains(r1.last)
  }

  println(part1(input))
  println(part2(input))
}