package be.cvalue.aoc2023

import be.cvalue.common.{Grid, ParseUtils, XY}
import be.cvalue.common.ParseUtils.*

import scala.collection.parallel.CollectionConverters.*
import be.cvalue.aoc2023.Day10.CellType.*

import scala.annotation.tailrec

object Day10 extends App {

  /*
    | is a vertical pipe connecting north and south.
    - is a horizontal pipe connecting east and west.
    L is a 90-degree bend connecting north and east.
    J is a 90-degree bend connecting north and west.
    7 is a 90-degree bend connecting south and west.
    F is a 90-degree bend connecting south and east.
    . is ground; there is no pipe in this tile.
    S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
   */
  enum CellType(val char: Char) {
    case Vert extends CellType('|')
    case Hor extends CellType('-')
    case NorthEast extends CellType('L')
    case NorthWest extends CellType('J')
    case SouthWest extends CellType('7')
    case SouthEast extends CellType('F')
    case Ground extends CellType('.')
    case Start extends CellType('S')
  }

  type Input = Grid[CellType]

  extension (grid: Grid[CellType]) def findConnectedPipes(xy: XY): List[XY] = {
    val cellType = grid.cellAt(xy)

    def checkUp(xy: XY): Option[XY] = {
      grid.maybeCellAt(xy.up).flatMap { up =>
        if (up == Vert || up == SouthEast || up == SouthWest) {
          Some(xy.up)
        } else {
          None
        }
      }
    }

    def checkDown(xy: XY): Option[XY] = {
      grid.maybeCellAt(xy.down).flatMap { down =>
        if (down == Vert || down == NorthEast || down == NorthWest) {
          Some(xy.down)
        } else {
          None
        }
      }
    }

    def checkLeft(xy: XY): Option[XY] = {
      grid.maybeCellAt(xy.left).flatMap { left =>
        if (left == Hor || left == NorthEast || left == SouthEast) {
          Some(xy.left)
        } else {
          None
        }
      }
    }

    def checkRight(xy: XY): Option[XY] = {
      grid.maybeCellAt(xy.right).flatMap { right =>
        if (right == Hor || right == NorthWest || right == SouthWest) {
          Some(xy.right)
        } else {
          None
        }
      }
    }

    val toCheck = cellType match
      case Ground => Nil
      case Vert => List(checkUp, checkDown)
      case Hor => List(checkLeft, checkRight)
      case NorthEast => List(checkUp, checkRight)
      case NorthWest => List(checkUp, checkLeft)
      case SouthWest => List(checkDown, checkLeft)
      case SouthEast => List(checkDown, checkRight)
      case Ground => Nil
      case Start => List(checkDown, checkLeft, checkRight, checkUp)

    toCheck.flatMap(_.apply(xy))
  }

  def parse(in: String): Input = {
    stringToCharGrid(in)
      .map(c =>
        CellType.values.find(_.char == c).get
      )
  }

  def part1(in: String): Long = {
    val grid = parse(in)
    val startPos = grid.allCells().find(_._2 == CellType.Start).get._1

    def followLoop(startPosition: XY) : List[XY] = {
      @tailrec
      def newConnected(loopSoFar: List[XY], xy:XY, visited:Set[XY]): List[XY] = {
        val connected = grid.findConnectedPipes(xy)
        val newConnections = connected.filterNot(visited.contains)
        if(newConnections.isEmpty) {
          // loop completed
          loopSoFar
        } else {
          newConnected(xy:: loopSoFar, newConnections.head, visited + newConnections.head)
        }
      }
      newConnected(Nil, startPosition, Set(startPosition))
    }
    val loop = followLoop(startPos)

    Math.ceil(followLoop(startPos).length / 2.0).toLong
  }

  def part2(in: String): Long = {
    val input = parse(in)
    ???
    //TODO CVF
  }

  println(part1(input))
  println(part2(input))


  lazy val input2 =
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin

  def input = ParseUtils.inputForDay(10)

}
