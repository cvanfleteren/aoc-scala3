package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

object Day11 extends App {

  enum CellType(val char: Char) {
    case Empty extends CellType('.')
    case Galaxy extends CellType('#')
  }

  type Input = Grid[CellType]

  extension (grid: Grid[CellType]) {
    def getGalaxies(): List[XY] = grid.allCells().filter(_._2 == CellType.Galaxy).map(_._1)
  }

  def parse(in: String): Input = {
    stringToCharGrid(in)
      .map(c =>
        CellType.values.find(_.char == c).get
      )
  }

  def calcDistance(grid: Grid[CellType], emptySpaceCost: Int): Long = {
    val emptyRowIndexes = grid.rows().zipWithIndex.filter { case (row, index) => !row.contains(CellType.Galaxy) }.map(_._2)
    val emptyColumnIndexes = grid.columns().zipWithIndex.filter { case (column, index) => !column.contains(CellType.Galaxy) }.map(_._2)

    grid.getGalaxies().combinations(2).map(_.sortBy(_.x)).map { case start :: finish :: Nil =>

      val moveX = finish.x - start.x
      val moveY = finish.y - start.y
      // wandel eerst langs de X-as (we starten altijd bij de kleinste X)
      val xDistance = (1 to moveX).map { step =>
        val newX = start.x + step
        val distance = if (emptyColumnIndexes.contains(newX)) {
          emptySpaceCost
        } else {
          1
        }
        distance.toLong
      }
      // waar zijn we nu geraakt? Vanaf hier gaan we de Y as doen
      val currentlyAt = start.copy(x = start.x + moveX)

      // wandel nu langs de Y as (maar kan zowel naar boven als beneden zijn)
      val yDistance = {
        val startForY :: finishForY :: Nil = List(start, finish).sortBy(_.y): @unchecked
        (1 to Math.abs(moveY)).map { step =>
          val newY = if (moveY >= 0) {
            currentlyAt.y + step
          } else {
            currentlyAt.y + (-1 * step)
          }
          val distance = if (emptyRowIndexes.contains(newY)) {
            emptySpaceCost
          } else {
            1
          }
          distance.toLong
        }
      }

      val distance = xDistance.sum + yDistance.sum

      distance
    }.sum
  }


  def part1(in: String): Long = {
    val grid = parse(in)
    calcDistance(grid, 2)
  }

  def part2(in: String): Long = {
    val expansionSize = 1000000
    val grid = parse(in)
    calcDistance(grid, expansionSize)
  }

  //9521550
  //298932923702
  println(part1(input))
  println(part2(input))


  lazy val input2 =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin

  def input = ParseUtils.inputForDay(11)

}
