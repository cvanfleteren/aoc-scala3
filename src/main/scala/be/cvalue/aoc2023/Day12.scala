package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}

object Day12 extends App {

  def numberOfSpringConfigurations(line: String) = {
    val (springs, damageGroupSizes) = parseLine(line)

    val memo = scala.collection.mutable.Map[(String, Seq[Int]), Long]()

    def arrangements(line: String, sizes: Seq[Int]): Long = {
      val memoValue = memo.get((line, sizes))

      memoValue match
        case Some(value) => value
        case None =>
          val result = {

            if (sizes.isEmpty) {
              if (line.contains('#')) 0L else 1L
            } else if (line.length < sizes.head) {
              0L
            } else {
              line.head match {
                case '.' => arrangements(line.tail, sizes)
                case '#' =>
                  val (sizeBlock, rest) = line.splitAt(sizes.head)
                  if (sizeBlock.contains('.') || rest.headOption.contains('#')) {
                    0L
                  } else {
                    arrangements('.' +: rest.tail, sizes.tail)
                  }
                case '?' =>
                  arrangements('.' +: line.tail, sizes) +
                    arrangements('#' +: line.tail, sizes)
              }
            }
          }

          memo((line, sizes)) = result
          result
    }

    arrangements(springs, damageGroupSizes)
  }

  def parseLine(line: String): (String, List[Int]) = {
    val s"$springs $constraints" = line: @unchecked
    val damageGroupSizes = constraints.split(',').map(_.toInt)
    (springs, damageGroupSizes.toList)
  }

  def unfold(line: String): String = {
    val s"$springs $constraints" = line: @unchecked
    val unfoldedSprings = repeat(springs, 5).mkString("?")
    val unfoldedConstraints = repeat(constraints, 5).mkString(",")
    s"$unfoldedSprings $unfoldedConstraints"
  }

  def repeat[T](x: T, n: Int) = Iterator.continually(x).take(n)


  def part1(input: String): Long = {
    input.linesIterator.map(numberOfSpringConfigurations).sum
  }

  def part2(input: String): Long = {
    input.linesIterator.map(unfold).map(numberOfSpringConfigurations).sum
  }


  //7195
  //33992866292225
  println(part1(input))
  println(part2(input))


  lazy val input2 =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  def input = ParseUtils.inputForDay(12)

}
