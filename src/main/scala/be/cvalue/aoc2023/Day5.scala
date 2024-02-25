package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

import scala.collection.immutable.NumericRange


object Day5 extends App {

  case class Input(
                    seeds: List[NumericRange.Inclusive[Long]] = Nil,
                    ranges: List[List[Ranges]] = Nil
                  ) {


    def calcLocation(source: Long): Long = {
      ranges.foldLeft(source) { (from, next) =>
        next.foldLeft(None: Option[Long]) { (f, r) =>
          f match {
            case Some(_) =>
              f
            case None =>
              r.destination(from)
          }
        }.getOrElse(from)
      }
    }
  }

  case class Ranges(destinationRange:Long, sourceRange:Long, rangeLength:Long) {

    def destination(source:Long) : Option[Long] = {
     if(sourceRange <= source && sourceRange + rangeLength > source) {
       Some(destinationRange + source - sourceRange)
     } else{
       None
     }
    }

  }

  def parse(in:String, seedsAsPairs:Boolean = false): Input = {
    val lines = ParseUtils.stringToLines(in)
    val seeds = if(! seedsAsPairs) {
      lines.head.split(' ').tail.map(_.toLong).toList.map(s => s.to(s))
    } else {
      lines.head.split(' ').tail.map(_.toLong).toList.sliding(2,2).map { case begin :: end :: Nil => begin.to(begin + end)
      }.toList
    }

    case class State(currentRange: List[Ranges] = Nil, ranges: List[List[Ranges]] = Nil)
    val res = lines.filter(_.trim.nonEmpty).tail.foldLeft(State()) { (state, line) =>
      if(line.contains("map:")) {
        if(state.currentRange.nonEmpty) {
          state.copy(ranges = state.currentRange.reverse :: state.ranges, currentRange = Nil)
        } else {
          state
        }
      } else {
        val ranges = line.split(' ').map(_.trim.toLong).toList match {
          case a :: b :: c :: Nil => Ranges(a,b,c)
        }
        state.copy(currentRange = ranges :: state.currentRange)
      }
    }
    val withLast = res.copy(ranges = res.currentRange.reverse ::res.ranges)
    Input(seeds, withLast.ranges.reverse)
  }

  def part1(input: String): Long = {
    val problem = parse(input)
    println(problem)

    problem.seeds.flatMap(r => r.map(problem.calcLocation)).min
  }

  def part2(input: String): Long = {
    val problem = parse(input, seedsAsPairs = true)

    //don't check every seed in the range
    // but check all seed that are:
    // - begin of range
    // - end of range
    // - begin of one of the ranges in the maps
    // - end of one of the ranges in the maps

    val allSeedsWithinMaps = problem.ranges.flatten.flatMap { range =>
      List(range.sourceRange, range.sourceRange + range.rangeLength)
    }
    val seeds = problem.seeds.flatMap(r => List(r.start, r.end)) ++ allSeedsWithinMaps

    seeds.map(problem.calcLocation).min
  }

  println(part1(input)) //175622908
  println(part2(input)) //5200543

  lazy val input2 =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  def input = ParseUtils.inputForDay(5)

}
