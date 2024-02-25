package be.cvalue.common


import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala


object ParseUtils {

  def lineToNumbers(sep:Char = ' ')(in: String): List[Long] = {
    in.split(sep).toList.map(_.toLong)
  }

  def stringToLines(in: String): List[String] = {
    in.linesIterator.toList
  }

  def stringToCharGrid(in: String): Grid[Char] = {
    val arrays = stringToLines(in).map(line => line.toVector).toVector
    Grid(arrays)
  }

  def inputForDay(day: Int): String = {
    val resource = Paths.get(this.getClass.getClassLoader.getResource(s"2023/day$day.txt").toURI)
    Files.readString(resource)
  }


}
