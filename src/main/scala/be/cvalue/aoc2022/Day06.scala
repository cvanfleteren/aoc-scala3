import Day02.Choice.*
import Day02.Result.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object Day06 extends App {

  val testInput =
    s"""mjqjpqmgbljsphdztnvjfqwrcgsmlb""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day06.txt"))

  def findMarker(in: String, markerSize: Int): Int = {
    val res = in.toList.sliding(markerSize).zipWithIndex.collectFirst {
      case (chars, index) if chars.toSet.size == markerSize => index + markerSize
    }

    res.get
  }

  def part1(in: String): String = {
    findMarker(in, 4).toString
  }

  def part2(in: String): String = {
    findMarker(in, 14).toString
  }

  println(part1(input))
  println(part2(input))
}