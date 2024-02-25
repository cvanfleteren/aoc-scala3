package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

import scala.collection.mutable

object Day14 extends App {

  type Input = Grid[Char]

  def moveRocksNorth(grid: Grid[Char]) = {
    case class State(column: List[Char], canShift: Boolean = false, amount: Int = 0, y: Int = 0)
    // ga kijken hoeveel we de ronde rotsen kunnen opschuiven
    val newColumns = grid.columns().zipWithIndex.map { case (column, x) =>
      val newColumn = column.zipWithIndex.foldLeft(State(column)) { case (state, (_, y)) =>
        val content = state.column(y)
        content match {
          // lege plek, dus kunnen 1 extra opschuiven
          case '.' =>
            state.copy(canShift = true, amount = state.amount + 1)
          // bezet door andere rots, kunnen niet opschuiven
          case '#' =>
            state.copy(canShift = false, amount = 0)
          // ronde rots, waarvoor er lege plek is, schuif ze op
          case 'O' if state.canShift =>
            state.copy(column = state.column.updated(y, '.').updated(y - state.amount, 'O'))
          // ronde rots, maar er was geen plek om die op te schuiven (eerste rij of een rots ervoor)
          case 'O' =>
            state
        }
      }.column
      newColumn
    }
    Grid.fromColumns(newColumns)
  }

  def calcLoad(grid: Grid[Char]): Int = {
    // eerste rij 10 punten, 2de 9, ... indien er 10 rijen zijn
    grid.columns().map { column =>
      column
        .zipWithIndex
        .map {
          case ('O', y) => grid.rows().size - y // enkel ronde rotsen zijn punten waard
          case _ => 0
        }
        .sum
    }.sum

  }

  def shakeCycle(grid: Grid[Char]): Grid[Char] = {

    1.to(4).foldLeft(grid) { case (grid, _) =>
      val moved = moveRocksNorth(grid)
      val rotated = moved.rotatedCW
      rotated
    }
  }


  def part1(input: String): Int = {
    val grid = ParseUtils.stringToCharGrid(input)
    val movedToNorth = moveRocksNorth(grid)
    val load = calcLoad(movedToNorth)
    load

  }

  def part2(input: String): Int = {

    val grid = ParseUtils.stringToCharGrid(input)

    val cache = new mutable.HashMap[Grid[Char], Int]()
    case class Cycle(from: Int, length: Int)
    // zoek naar een cycle
    // daardoor moeten we niet alle iteraties doorlopen als we toch cycles hebben
    val cycle = Seq.unfold((shakeCycle(grid), 1, false)) { case (grid, i, found) =>
      if (found) {
        None
      } else {
        if (cache.contains(grid)) {
          // we hebben een cycle gedetecteerd,
          // nu weten we hoe lang de cycle is en vanaf wanneer die begint
          Some((Cycle(cache(grid), i - cache(grid)), (grid, cache(grid), true)))
        } else {
          val cycled = shakeCycle(grid)
          cache.put(grid, i)
          Some((Cycle(i, i), (cycled, i + 1, false)))
        }
      }
    }.last

    // we kennen de cycle lengte, skip zoveel mogelijk iteraties van de cycle
    val tot = ((1_000_000_000 / cycle.length) - 1) * cycle.length

    val finalGrid = 1.to(1_000_000_000 - tot).foldLeft(grid) { case (grid, _) =>
      shakeCycle(grid)
    }

    val load = calcLoad(finalGrid)
    load
  }

  //103614
  println(part1(input))
  //83790
  println(part2(input))


  def testInput =
    s"""O....#....
       |O.OO#....#
       |.....##...
       |OO.#O....O
       |.O.....O#.
       |O.#..O.#.#
       |..O..#O..O
       |.......O..
       |#....###..
       |#OO..#....""".stripMargin


  def input = ParseUtils.inputForDay(14)

}
