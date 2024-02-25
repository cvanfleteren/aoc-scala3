package be.cvalue.aoc2023

import be.cvalue.common.{Grid, ParseUtils, XY}
import be.cvalue.common.ParseUtils.*

object Day13 extends App {

  type Input = Grid[Char]

  def parse(in:String): List[Input] = {
    val (inputs, _) = in.linesIterator.foldLeft((List.empty[Input], List.empty[String])) { case ((inputs, lines),line) =>
      if(line.trim.isEmpty) {
        (Grid.fromRows(lines.reverse.map(_.toList))::inputs, Nil)
      } else {
        (inputs, line::lines)
      }
    }
    inputs.reverse
  }

  def findMirror(grid: Input): Option[Int] = {
    val columns = grid.columns()
    val columnsToCheck = columns.zipWithIndex.map(_._2)

    val mirrorIndexes = columnsToCheck.filter { index =>
      val (left, right) = columns.splitAt(index)
      val smallestSize = Math.min(left.size, right.size)
      val checkLeft = left.takeRight(smallestSize)
      val checkRight = right.take(smallestSize).reverse
      checkLeft == checkRight
    }
    mirrorIndexes.lastOption
  }

  def part1(input: String): Int = {
    val grids = parse(input)

    val total = grids.map { grid =>
      val columns = findMirror(grid)
      // flip grid to check rows instead of columns
      val rows = findMirror(Grid.fromRows(grid.rows().transpose))
      columns.getOrElse(0) + rows.map(_ * 100).getOrElse(0)
    }.sum

    total
  }

  def part2(input: String): Int = {
    val grids = parse(input)

    val total = grids.map { grid =>

      val columnReflection = findMirror(grid).filter(_ != 0)
      // flip grid to check rows instead of columns
      val rowReflection = findMirror(Grid.fromRows(grid.rows().transpose)).filter(_ != 0)

      println(s"Column: $columnReflection, row: $rowReflection")
      //try with smudges
      val altColumnRef = {
        for {
          x <- grid.columns().indices
          y <- grid.rows().indices
          refl = {
            val cur = grid.cellAt(XY(x,y))
            val opposite = cur match {
              case '#' => '.'
              case '.' => '#'
            }
            findMirror(grid.withValue(XY(x,y),opposite))
          }  if refl != columnReflection && !refl.contains(0)
        } yield refl
      }.flatten

      val altRowRef = {
        val g2 = Grid.fromRows(grid.rows().transpose)
        for {
          x <- g2.columns().indices
          y <- g2.rows().indices
          refl = {
            val cur = g2.cellAt(XY(x, y))
            val opposite = cur match {
              case '#' => '.'
              case '.' => '#'
            }
            findMirror(g2.withValue(XY(x, y), opposite))
          } if refl != rowReflection && !refl.contains(0)
        } yield refl
      }.flatten

      println(s"Fixed: Column: $altColumnRef, row: $altRowRef")
      altColumnRef.lastOption.getOrElse(0) + altRowRef.lastOption.map(_ * 100).getOrElse(0)
    }.sum

    total
  }

//  println(part1(input))
//  println(part1(input2))
//  println(part1(input3))
  println(part2(input4))

  lazy val input2 =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#
      |
      |.#.##.#.#
      |.##..##..
      |.#.##.#..
      |#......##
      |#......##
      |.#.##.#..
      |.##..##.#
      |
      |#..#....#
      |###..##..
      |.##.#####
      |.##.#####
      |###..##..
      |#..#....#
      |#..##...#
      |
      |#.##..##.
      |..#.##.#.
      |##..#...#
      |##...#..#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |""".stripMargin

  lazy val input3 =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.
        |
        |#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#
        |
        |""".stripMargin

  lazy val input4 =
        """###.##.##
          |##.####.#
          |##.#..#.#
          |####..###
          |....##...
          |##.#..#.#
          |...#..#..
          |##..###.#
          |##......#
          |##......#
          |..#.##.#.
          |...#..#..
          |##.####.#
          |....##...
          |...####..
          |....##...
          |##.####.#
          |
          |.##.##...##...##.
          |#####..##..##..##
          |.....##..##..##..
          |.##.#.#.####.#.#.
          |.##...#.#..#.#...
          |....#..........#.
          |#..#..#......#..#
          |....###.....####.
          |.##...#.#..#.#...
          |.....#..####..#..
          |#..#...##..##...#
          |....#...#..#...#.
          |#..#.##########.#
          |#..##...####...##
          |#####.##.##.##.##
          |
          |""".stripMargin


  def input = ParseUtils.inputForDay(13)

}
