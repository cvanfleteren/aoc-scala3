package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils}

object Day15 extends App {

  case class Instruction(label: String, operation: Char, focalLength: Int) {
    val box: Int = hash(label)
  }

  def hash(label: String): Int = {
    def hash(curVal: Int, char: Char): Int = {
      val ascii = char.toInt
      ((curVal + ascii) * 17) % 256
    }

    label.foldLeft(0)(hash)
  }



  def part1(input: String): Int = {
    input.split(',').toList.map(hash).sum
  }

  def part2(input: String): Int = {

    def score(boxes: Vector[Vector[Instruction]]): Int = {
      // multiply:
      // One plus the box number of the lens in question.
      // The slot number of the lens within the box: 1 for the first lens, 2 for the second lens, and so on.
      // The focal length of the lens.
      boxes.zipWithIndex.map { case (box, index) =>
        box.zipWithIndex.map { case (lens, boxIndex) => (1 + index) * (boxIndex + 1) * lens.focalLength }.sum
      }.sum
    }

    val instructions = input.split(',').toList.map {
      case s"$label=$focalLength" => Instruction(label, '=', focalLength.toInt)
      case s"$label-" => Instruction(label, '-', 0)
    }

    val boxes = instructions.foldLeft(Vector.fill(256)(Vector.empty[Instruction])) { case (boxes, instruction) =>
      val boxIdx = instruction.box
      val updatedBoxes = instruction.operation match {
        //      If the operation character is a dash (-), go to the relevant box and remove the lens with the given label if it is present in the box.
        //      Then, move any remaining lenses as far forward in the box as they can go without changing their order, filling any space made by removing the indicated lens. (If no lens in that box has the given label, nothing happens.)
        case '-' =>
          val box = boxes(boxIdx).filter(_.label != instruction.label)
          boxes.updated(boxIdx, box)

        //If the operation character is an equals sign (=), it will be followed by a number indicating the focal length of the lens that needs to go into the relevant box; be sure to use the label maker to mark the lens with the label given in the beginning of the step so you can find it later. There are two possible situations:
        case '=' =>
          val box = boxes(boxIdx)
          val updatedBox = box.indexWhere(_.label == instruction.label) match {
            //If there is not already a lens in the box with the same label, add the lens to the box immediately behind any lenses already in the box. Don't move any of the other lenses when you do this. If there aren't any lenses in the box, the new lens goes all the way to the front of the box.
            case -1 => box.appended(instruction)
            //If there is already a lens in the box with the same label, replace the old lens with the new lens: remove the old lens and put the new lens in its place, not moving any other lenses in the box.
            case idx => box.updated(idx, instruction)
          }
          boxes.updated(boxIdx, updatedBox)
      }
      updatedBoxes
    }
    println(boxes.filter(_.nonEmpty).mkString("\n"))
    score(boxes)
  }

  //511416
  println(part1(testInput))
  //290779
  println(part2(input))


  def testInput =
    s"""rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""".stripMargin


  def input = ParseUtils.inputForDay(15)

}
