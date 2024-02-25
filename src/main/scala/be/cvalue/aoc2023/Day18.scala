package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

object Day18 extends App {


  case class Instruction(dir: Char, amount: Int)

  case class State(maxX: Int = 0, maxY: Int = 0, curX: Int = 0, curY: Int = 0, minX: Int = 0, minY: Int = 0) {
    def r(amount: Int) = copy(curX = curX + amount, maxX = Math.max(maxX, curX + amount))

    def l(amount: Int) = copy(curX = curX - amount, minX = Math.min(minX, curX - amount))

    def u(amount: Int) = copy(curY = curY - amount, minY = Math.min(minY, curY - amount))

    def d(amount: Int) = copy(curY = curY + amount, maxY = Math.max(maxY, curY + amount))
  }


  def gridSize(instructions: List[Instruction]): State = {
    val init = State()
    val res = instructions.foldLeft(init) { case (state, instruction) =>
      instruction.dir match {
        case 'R' => state.r(instruction.amount)
        case 'L' => state.l(instruction.amount)
        case 'U' => state.u(instruction.amount)
        case 'D' => state.d(instruction.amount)
      }
    }
    res
  }


  def digout(grid: Grid[Char]): Grid[Char] = {
      val startEmptyPos: XY = grid.rows().zipWithIndex.find(_._1.contains('#')).map { case (row, y) =>
        XY(row.indexWhere(_ == '#') +1, y +1)
      }.get

      // flood fill all the neighbours that are also empty
      val visited : Set[XY] = Set.empty
      val neighbours = startEmptyPos.horAndVerNeighboursWithinGrid(grid)

      def fillIn(pos:XY, grid: Grid[Char], visited: Set[XY]): (Grid[Char], Set[XY]) = {
        if(visited.contains(pos)) {
          (grid, visited)
        } else if(grid.cellAt(pos) == '.') {
          pos.horAndVerNeighboursWithinGrid(grid).filter(p => !visited.contains(p)).foldLeft((grid.withValue(pos, '#'), visited + pos)) { case ((grid, visited),  pos) =>
            fillIn(pos, grid, visited)
          }
        } else {
          (grid, visited)
        }
      }

    fillIn(startEmptyPos, grid, Set.empty)._1
  }

  def areaWithShoelace(instructions: List[Instruction]): Long = {
    case class State(area: Long = 0, prevRow: Long = 0, prevCol: Long = 0)

    instructions.foldLeft(State()) { case (state, instruction) =>
      val amount = instruction.amount

      val (row, col) = instruction.dir match {
        case 'R' => (state.prevRow, state.prevCol + amount)
        case 'L' => (state.prevRow, state.prevCol - amount)
        case 'U' => (state.prevRow - amount, state.prevCol)
        case 'D' => (state.prevRow + amount, state.prevCol)
      }

      val area = state.area + (state.prevCol * row - state.prevRow * col) + amount

      state.copy(area = area, prevRow = row, prevCol = col)
    }.area / 2 + 1
  }

  def part1(input: String): Int = {
    def parseLine(line: String): Instruction = line match {
      case s"$dir $amount ($_)" => Instruction(dir.charAt(0), amount.toInt)
    }

    val instructions = input.linesIterator.map(parseLine).toList
    val state = gridSize(instructions)
    // y/x is 0 based, atl rijen / kolommen +1 dus
    val grid = Grid.fromRows(List.fill(state.maxY + 1 + state.minY.abs, state.maxX + 1 + state.minX.abs)('.'))
    val beginPos = XY(state.minX.abs, state.minY.abs)

    val gridWithTrenches = instructions.foldLeft((grid, beginPos)) { case ((grid, pos), instruction) =>
      instruction.dir match {
        case 'R' => 1.to(instruction.amount).foldLeft((grid, pos)) { case ((grid, pos), _) =>
          val moved = pos.right
          (grid.withValue(moved, '#'), moved)
        }
        case 'L' => 1.to(instruction.amount).foldLeft((grid, pos)) { case ((grid, pos), _) =>
          val moved = pos.left
          (grid.withValue(moved, '#'), moved)
        }
        case 'U' => 1.to(instruction.amount).foldLeft((grid, pos)) { case ((grid, pos), _) =>
          val moved = pos.up
          (grid.withValue(moved, '#'), moved)
        }
        case 'D' => 1.to(instruction.amount).foldLeft((grid, pos)) { case ((grid, pos), _) =>
          val moved = pos.down
          (grid.withValue(moved, '#'), moved)
        }
      }
    }._1

    digout(gridWithTrenches).allCells().count(_._2 == '#')
  }

  def part1Revisited(input:String): Long = {
    def parseLine(line: String): Instruction = line match {
      case s"$dir $amount ($_)" => Instruction(dir.charAt(0), amount.toInt)
    }

    val instructions = input.linesIterator.map(parseLine).toList

    areaWithShoelace(instructions)
  }

  def part2(input: String): Long = {


    def parseLine(line: String): Instruction = line match {
      case s"$_ $_ (#$hex)" =>
        val amount = Integer.parseInt(hex.take(5).mkString,16)
        val dir = hex.last match {
          case '0' => 'R'
          case '1' => 'D'
          case '2' => 'L'
          case '3' => 'U'
        }
        Instruction(dir, amount)
    }
    val instructions = input.linesIterator.map(parseLine).toList

    areaWithShoelace(instructions)
  }

  //34329
  println(part1Revisited(input))
  //42617947302920
  println(part2(input))

  lazy val testInput =
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin

  def input = ParseUtils.inputForDay(18)


}
