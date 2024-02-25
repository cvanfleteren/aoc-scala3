package be.cvalue.aoc2023

import be.cvalue.aoc2023.Day16.Heading.{North, *}
import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

object Day16 extends App {

  type Input = Grid[Char]


  enum Heading(val char: Char) {
    case North extends Heading('^')
    case East extends Heading('>')
    case South extends Heading('v')
    case West extends Heading('<')
  }

  case class Movement(from: XY, heading: Heading)


  def possibleHeadingsFrom(heading: Heading, to: XY, value: Char): List[Movement] = {
    val newHeadings = ((value, heading) match {
      case ('.', heading) => (to, heading) :: Nil
      case ('|', h@(North | South)) => (to, h) :: Nil
      case ('|', East | West) => (to, North) :: (to, South) :: Nil
      case ('-', North | South) => (to, East) :: (to, West) :: Nil
      case ('-', East | West) => (to, heading) :: Nil
      case ('\\', North) => (to, West) :: Nil
      case ('\\', East) => (to, South) :: Nil
      case ('\\', South) => (to, East) :: Nil
      case ('\\', West) => (to, North) :: Nil
      case ('/', North) => (to, East) :: Nil
      case ('/', East) => (to, North) :: Nil
      case ('/', South) => (to, West) :: Nil
      case ('/', West) => (to, South) :: Nil
    }).map(Movement.apply)
    newHeadings
  }


  def navigateGrid(grid: Input, startMovement: Movement): Int = {
    println(s"start from $startMovement")

    def navigate(movement: Movement, alreadyDone: Set[Movement]): Set[Movement] = {
      //println(s"${movement.from.x},${movement.from.y}: ${movement.heading.char} ")
      val from = movement.from
      val updated = grid.withValue(from, 'X')
      val cell = grid.cellAt(from)
      val to = movement.heading match {
        case Heading.North => from.up
        case Heading.East => from.right
        case Heading.South => from.down
        case Heading.West => from.left
      }

      val newHeadings = grid.maybeCellAt(to) match {
        case Some(value) =>
          val newHeadings = possibleHeadingsFrom(movement.heading, to, value)
          newHeadings
        case None =>
         // println(s"stop at $from, going out of bounds if going to ${movement.heading.char}")
          Nil
      }

      newHeadings.foldLeft(alreadyDone) { case (accum, m) =>
        if (accum.contains(m)) {
          accum
        } else {
          navigate(m, accum + m)
        }
      }
    }

    val all = navigate(startMovement, Set(startMovement))
    all.map(_.from).size
  }

  def calcStartMovement(grid: Input)(pos: XY) = {
    val cell = grid.cellAt(pos)
    val heading = cell match {
      case ('.') => Some(East)
      case ('\\') => Some(South)
      case ('|') => None
      case ('-') => Some(East)
      case ('/') => Some(North)
    }
    heading.map(Movement(pos, _))
  }

  def part1(input: String): Int = {
    val grid = ParseUtils.stringToCharGrid(input)

    navigateGrid(grid, calcStartMovement(grid)(XY(0,0)).get)
  }

  def part2(input: String): Int = {
    val grid = ParseUtils.stringToCharGrid(input)

    val rows = grid.rows().size -1
    val cols = grid.columns().size -1

    // voor deel 2 moeten we gewoon van elke mogelijk plek (langs buiten) starten en de max zoeken
    val top = grid.columns().indices.map(XY(_, 0)).flatMap(pos => possibleHeadingsFrom(Heading.South, pos ,grid.cellAt(pos)))
    val left = grid.rows().indices.map(XY(0, _)).flatMap(pos => possibleHeadingsFrom(Heading.East, pos ,grid.cellAt(pos)))
    val bottom = grid.columns().indices.map(XY(_,cols)).flatMap(pos => possibleHeadingsFrom(Heading.North, pos ,grid.cellAt(pos)))
    val right = grid.rows().indices.map(XY(cols, _)).flatMap(pos => possibleHeadingsFrom(Heading.West, pos ,grid.cellAt(pos)))

    val startCoords = top ++ left ++ bottom ++ right
    startCoords.map(navigateGrid(grid, _)).max
  }

  //needs -Xss2m!

  //6605
  println(part1(input))

  //6766
  println(part2(input))

  lazy val testInput =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin

  def input = ParseUtils.inputForDay(16)

}
