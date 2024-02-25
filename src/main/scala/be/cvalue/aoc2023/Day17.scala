package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

import scala.collection.mutable

object Day17 extends App {

  type Input = Grid[Int]


  def part1(input: String): Int = {
    val grid = ParseUtils.stringToCharGrid(input).map(_.toString.toInt)
    ???

  }

  def shortestPath(grid: Input): Int = {
    val costs = mutable.Map[XY, Int]()
    val curPos = XY(0,0)
    val curCost = grid.cellAt(curPos)
    costs.put(curPos, grid.cellAt(curPos))

    val neighbours = curPos.horAndVerNeighboursWithinGrid(grid)
    neighbours.foreach { pos =>
      costs.put(pos, curCost + grid.cellAt(pos))
    }
    ???
  }

  def part2(input: String): Int = {
    val grid = ParseUtils.stringToCharGrid(input)
    ???
  }

  println(part1(input))
  println(part2(input))

  lazy val testInput =
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin

  def input = ParseUtils.inputForDay(17)

}
