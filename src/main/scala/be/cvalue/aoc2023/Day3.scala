package be.cvalue.aoc2023

import be.cvalue.common.{XY, Grid, ParseUtils}
import be.cvalue.common.ParseUtils.*

object Day3 extends App {
  //TODO CVF fix met juiste grid coords
  def isSymbol(char:Char): Boolean =  !char.isDigit && char != '.'

  def part1(input: String): Int = {
    val grid = stringToCharGrid(input)
    // look for digits on the grid that have a symbol next to it
    val adjacentToSymbols = grid.allCells().flatMap { case (coord, cell) =>
      if(cell.isDigit && grid.adjacentCells(coord).exists(isSymbol)) {
        Some(coord)
      } else {
        None
      }
    }

    // we now have all the cells that have a symbol next to them
    // but we're missing the numbers next to them
    // loop over the found coords, and try to make the numbers
    case class State(found: Set[XY] = Set.empty, number:List[Int] = List.empty)
    val res = adjacentToSymbols.foldLeft(State()) { case (state: State, coord: XY) =>
      if(state.found.contains(coord)) {
        state
      } else {
        val (number, usedCoords) = getNumberAt(coord, grid)
        state.copy(number = number :: state.number, found = state.found ++ usedCoords)
      }
    }

    res.number.sum
  }

  def getNumberAt(xy: XY, matrix: Grid[Char]): (Int, Set[XY]) = {
    // look for a sequence of digits before / after the xy, keeping track of the coords at which we found digits
    val (before, after) = matrix.withXY.rowOf(xy).splitAt(xy.x)
    val digitsBefore = before.reverse.takeWhile(_._2.isDigit).reverse//from right-to-left
    val digitsAfter = after.takeWhile(_._2.isDigit)//left to right
    val usedCoords = digitsAfter.map(_._1) ::: digitsBefore.map(_._1)

    val number = (digitsBefore.map(_._2) ::: digitsAfter.map(_._2)).mkString.toInt
    (number, usedCoords.toSet)
  }

  def part2(input: String): Int = {
    val matrix = stringToCharGrid(input)
    val starCoords = matrix.allCells().flatMap { case (coord, cell) =>
     if(isSymbol(cell)) {
       val neighbourDigits = coord.neighbours().filter(coord => matrix.cellAt(coord).isDigit)

       // check all digits for unique numbers, like we did in part 1
       case class State(found: Set[XY] = Set.empty, number:List[Int] = List.empty)
       val res = neighbourDigits.foldLeft(State()) { case (state: State, coord: XY) =>
         if (state.found.contains(coord)) {
           state
         } else {
           val (number, usedCoords) = getNumberAt(coord, matrix)
           state.copy(number = number :: state.number, found = state.found ++ usedCoords)
         }
       }
       if(res.number.size == 2) {
         Some(res.number.head * res.number.last)
       } else {
         None
       }
     } else {
       None
     }
    }

    starCoords.sum
  }

  //538046
  //81709807
  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  def input = ParseUtils.inputForDay(3)

}
