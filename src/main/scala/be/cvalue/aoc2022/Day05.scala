import Day02.Choice.*
import Day02.Result.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object Day05 extends App {

  val testInput =
    s"""    [D]
       |[N] [C]
       |[Z] [M] [P]
       | 1   2   3
       |
       |move 1 from 2 to 1
       |move 3 from 1 to 3
       |move 2 from 2 to 1
       |move 1 from 1 to 2""".stripMargin

  val input = Files.readString(Path.of("./src/main/resources/2022/day05.txt"))

  type Stacks = List[List[String]]

  case class Instruction(count: Int, fromStack: Int, toStack: Int) {
    override def toString: String = {
      s"move $count from $fromStack to $toStack"
    }
  }

  def parse(in: String): (Stacks, List[Instruction]) = {
    val lines = in.split('\n').toList

    val (instructionLines, config) = lines.partition(_.startsWith("move"))

    val instructions = instructionLines.map(parseInstruction)
    val createConfig = parseCrateConfig(config.filter(_.nonEmpty))
    (createConfig, instructions)
  }

  def parseCrateConfig(value: List[String]): Stacks = {
    val crateCount = value.last.filter(_.isDigit).last.toString.toInt

    val stacks = List.fill(crateCount)(List.empty[String])
    val res = value.dropRight(1).foldLeft(stacks) { case (stacks, line) =>
      val columns = Range(0, crateCount).zip(line.grouped(4))
      columns.filter(_._2.trim.nonEmpty).foldLeft(stacks) { case (stacks, (stackIndex, crate)) =>
        stacks.updated(stackIndex, crate.filterNot(_.isWhitespace) :: stacks(stackIndex))
      }
    }

    res.map(_.reverse)
  }

  def parseInstruction(line: String): Instruction = {
    val parts = line.split(' ')

    Instruction(parts(1).toInt, parts(3).toInt, parts(5).toInt)
  }

  def followInstructionPart1(instruction: Instruction, stacks: Stacks): Stacks = {
    val from = stacks(instruction.fromStack - 1)
    val to = stacks(instruction.toStack - 1)

    val (newFrom, newTo) = (0 until instruction.count).foldLeft((from, to)) { case ((from, to), _) =>
      val elem = from.head
      (from.tail, elem :: to)
    }

    val res = stacks.updated(instruction.fromStack - 1, newFrom).updated(instruction.toStack - 1, newTo)
    res
  }

  def followInstructionPart2(instruction: Instruction, stacks: Stacks): Stacks = {
    val from = stacks(instruction.fromStack - 1)
    val to = stacks(instruction.toStack - 1)

    val (move, keep) = from.splitAt(instruction.count)

    val res = stacks.updated(instruction.fromStack - 1, keep).updated(instruction.toStack - 1, move ::: to)
    res
  }

  def printStacks(stacks: Stacks): Unit = {
    println(stacks.map(_.reverse.mkString(" ")).mkString("\n"))
  }

  def part1(in: String): String = {
    val parsed = parse(in)
    val res = parsed._2.foldLeft(parsed._1) { case (stacks, instruction) =>
      val res = followInstructionPart1(instruction, stacks)
      res
    }

    makeAnswer(res)
  }

  def part2(in: String): String = {
    val parsed = parse(in)
    val res = parsed._2.foldLeft(parsed._1) { case (stacks, instruction) =>
      followInstructionPart2(instruction, stacks)
    }

    makeAnswer(res)
  }

  def makeAnswer(res: Stacks) = {
    res.map(_.head.filter(_.isLetter)).mkString("")
  }

  println(part1(input))
  println(part2(input))
}