package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

import scala.collection.parallel.CollectionConverters.*

object Day8 extends App {

  type LR = (String, String)
  type Input = (List[Char], Map[String, LR])


  def parse(in:String): Input = {
    ParseUtils.stringToLines(in) match {
      case head :: empty :: tail => (head.toList, tail.map(parseNodeLine).toMap)
    }
  }

  def parseNodeLine(line: String): (String,LR) = {
    line.split('=').toList match {
      case name :: lr :: Nil => {
        val (l,r) = lr.trim.replace("(","").replace(")","").split(',').toList match {
          case l :: r :: Nil => (l.trim, r.trim)
        }
        name.trim -> (l, r)
      }
    }
  }

  def part1(in: String): Long = {
    val input = parse(in)

    case class State(currentKey:String="AAA")

    val attempts = Seq.unfold(State()) { state =>
      if(state.currentKey == "ZZZ") {
        None
      } else {
        val nextKey = input._1.foldLeft(state.currentKey) { (key, instruction) =>
          val lr = input._2(key)
          val next = if (instruction == 'L') lr._1 else lr._2
          next
        }
        Some(((),state.copy(currentKey = nextKey)))
      }
    }
    attempts.size * input._1.size
  }

  def lcm(ints:Seq[Long]): Long = {
    ints.reduce(lcm)
  }

  def lcm(a:Long, b:Long): Long = {
    Math.abs(a*b) / gcd(a, b)
  }

  def gcd(a:Long, b:Long): Long = {
    if(b == 0) a else gcd(b, a % b)
  }

  def part2(in: String): Long = {
    val input = parse(in)

    val startingLocs = input._2.keys.filter(_.endsWith("A"))
    val perLoc = startingLocs.toList.par.map { start =>
      case class State(currentKey: String = start)

      val attempts = Seq.unfold(State()) { state =>
        if (state.currentKey.endsWith("Z")) {
          None
        } else {
          val nextKey = input._1.foldLeft(state.currentKey) { (key, instruction) =>
            val lr = input._2(key)
            val next = if (instruction == 'L') lr._1 else lr._2
            next
          }
          Some(((), state.copy(currentKey = nextKey)))
        }
      }
      attempts.size * input._1.size
    }

    lcm(perLoc.toList.map(_.toLong))
  }
  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  lazy val input3 =
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin

  def input = ParseUtils.inputForDay(8)

}
