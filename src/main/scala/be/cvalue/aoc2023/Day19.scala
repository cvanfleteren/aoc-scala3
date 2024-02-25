package be.cvalue.aoc2023

import be.cvalue.common.ParseUtils.*
import be.cvalue.common.{Grid, ParseUtils, XY}
import be.cvalue.aoc2023.Day19.Comp._

object Day19 extends App {

  enum Comp {
    case LT
    case GT
  }

  case class Item(x: Int, m: Int, a: Int, s: Int) {
    def sum: Int = x + m + a + s
  }

  case class Workflow(name: String, checks: List[Check], forward: Forward) {
    def eval(item: Item, workflows: Map[String, Workflow]): String = {
      val dest = checks.foldLeft(Option.empty[String]) { case (destination, check) =>
        destination match
          case Some(x) => Some(x)
          case None =>
            val passes = check.passes(item)
            if (passes) {
              Some(check.action)
            } else {
              None
            }
      }
      val name = dest.getOrElse(forward.name)
      val workflow = workflows.get(name) match {
        case Some(wf) => wf.eval(item, workflows)
        case None => name
      }
      workflow
    }
  }

  case class Check(property: String, amount: Int, comp: Comp, action: String) {
    def passes(item: Item): Boolean = {
      val value = property match {
        case "x" => item.x
        case "m" => item.m
        case "a" => item.a
        case "s" => item.s
      }
      comp match {
        case LT => value < amount
        case GT => value > amount
      }
    }
  }

  case class Forward(name: String)

  def parseWorkflow(in: String): Workflow = {
    //px{a<2006:qkq,m>2090:A,rfg}
    in match {
      case s"$name{$checksS}" =>
        val parts = checksS.split(',')
        val checks = parts.init.map {
          case s"$property>$amount:$action" => Check(property, amount.toInt, GT, action)
          case s"$property<$amount:$action" => Check(property, amount.toInt, LT, action)
        }
        val forward = Forward(parts.last)

        Workflow(name, checks.toList, forward)
    }
  }

  def parseItem(in: String): Item = {
    //{x=787,m=2655,a=1222,s=2876}
    in match {
      case s"{x=$x,m=$m,a=$a,s=$s}" => Item(x = x.toInt, m = m.toInt, a = a.toInt, s = s.toInt)
    }
  }

  def parse(in: String): (Map[String, Workflow], List[Item]) = {
    val workflowsS :: itemsS :: Nil = in.split("\n\n").toList
    val workflows = workflowsS.linesIterator.map(parseWorkflow).map(w => w.name -> w).toMap
    val items = itemsS.linesIterator.map(parseItem).toList

    (workflows, items)
  }

  def part1(input: String): Int = {
    val (workflows, items) = parse(input)
    val in = workflows("in")
    val destinations = items.map { item =>
      (in.eval(item, workflows), item)
    }

    destinations.filter(_._1 == "A").map(_._2.sum).sum
  }

  def part2(input: String): Long = {
    val (workflows, items) = parse(input)
    val in = workflows("in")

    case class OkRanges(x: Option[Range] = None, m: Option[Range] = None, a: Option[Range] = None, s: Option[Range] = None) {
      def rangeOn: String = {
        if (x.isDefined) {
          "x"
        } else if (m.isDefined) {
          "m"
        } else if (a.isDefined) {
          "a"
        } else {
          "s"
        }
      }
    }

    val boundaries = workflows.values.flatMap(_.checks.map { check =>
      (check.property, check.comp) match {
        case ("x", LT) => OkRanges(x = Some(1.until(check.amount)))
        case ("m", LT) => OkRanges(m = Some(1.until(check.amount)))
        case ("a", LT) => OkRanges(a = Some(1.until(check.amount)))
        case ("s", LT) => OkRanges(s = Some(1.until(check.amount)))
        case ("x", GT) => OkRanges(x = Some((check.amount +1).to(4000)))
        case ("m", GT) => OkRanges(m = Some((check.amount +1).to(4000)))
        case ("a", GT) => OkRanges(a = Some((check.amount +1).to(4000)))
        case ("s", GT) => OkRanges(s = Some((check.amount +1).to(4000)))
      }
    }).toList

    val grouped = boundaries.groupBy(_.rangeOn)

    //println(grouped.mkString("\n"))

    val rangesCombinations = for {
      x <- grouped.getOrElse("x",List(OkRanges(x = Some(1.to(4000))))).map(_.x.get)
      m <- grouped.getOrElse("m",List(OkRanges(m = Some(1.to(4000))))).map(_.m.get)
      a <- grouped.getOrElse("a",List(OkRanges(a = Some(1.to(4000))))).map(_.a.get)
      s <- grouped.getOrElse("s",List(OkRanges(s = Some(1.to(4000))))).map(_.s.get)
    } yield {
      (x,m,a,s)
    }

    val s = rangesCombinations.flatMap { case (x,m,a,s) =>
      List(
        Item(x.head, m.head, a.head, s.head),
        Item(x.head, m.head, a.head, s.last),
        Item(x.head, m.head, a.last, s.head),
        Item(x.head, m.head, a.last, s.last),
        Item(x.head, m.last, a.head, s.head),
        Item(x.head, m.last, a.head, s.last),
        Item(x.head, m.last, a.last, s.head),
        Item(x.head, m.last, a.last, s.last),
        Item(x.last, m.head, a.head, s.head),
        Item(x.last, m.head, a.head, s.last),
        Item(x.last, m.head, a.last, s.head),
        Item(x.last, m.head, a.last, s.last),
        Item(x.last, m.last, a.head, s.head),
        Item(x.last, m.last, a.head, s.last),
        Item(x.last, m.last, a.last, s.head),
        Item(x.last, m.last, a.last, s.last),
      ).map { item =>
        (OkRanges(Some(x), Some(m), Some(a), Some(s)), in.eval(item, workflows))
      }.filter(_._2 == "A").map(_._1)
    }

    val all = s.toSet
    val sum = all.map {  range =>
        range.x.map(_.size.toLong).getOrElse(1L) * range.m.map(_.size.toLong).getOrElse(1L) *  range.a.map(_.size.toLong).getOrElse(1L) *  range.s.map(_.size.toLong).getOrElse(1L)
    }.sum

    sum
    //???
  }

  def passed(in: String, x: Int = 1, m: Int = 1, a: Int = 1, s: Int = 1): Boolean = {
    val workflows = in.linesIterator.toList.init.filter(_.nonEmpty).map(parseWorkflow).map(w => w.name -> w).toMap
    val item = Item(x, m, a, s)
    workflows("in").eval(item, workflows) == "A"
  }


  //353553
//  println(part1(input))
//  //167409079868000
//  //727356195363300
//
//  assert(part2(test) == 0)
//  assert(part2(test2) == 2000L * 4000 * 4000 * 4000)
//  assert(part2(test3) == 2000L * 2000 * 4000 * 4000)
//
//  println(part2(test3))
//  println(part2(test4))
  println(part2(test5))

  println(passed(test5, s=2))


  
  // alles wordt geweigerd
  lazy val test = s"""in{x<4000:R,m<4000:R,a<4000:R,s<4000:R,A}
                     |
                     |{x=1,m=1,a=1,s=1}""".stripMargin

  // enkel X onder 2001 worden aanvaard, m/a/s is niet relevant
  lazy val test2 =
    s"""in{x<2001:A,R}
       |
       |{x=1,m=1,a=1,s=1}""".stripMargin

  // enkel X onder 2001 of m onder 2000 worden aanvaard, a/s is niet relevant
  lazy val test3 =
    s"""in{x<2001:A,m<2001:A,R}
       |
       |{x=1,m=1,a=1,s=1}""".stripMargin

  // enkel X onder 2001 && m onder 2000 worden aanvaard, a/s is niet relevant
  lazy val test4 =
    s"""in{x<2001:BBB,R}
       |BBB{m<2001:A,R}
       |
       |{x=1,m=1,a=1,s=1}""".stripMargin

  lazy val test5 =
    s"""in{x<4000:R,m<4000:R,a<4000:R,A}
       |
       |{x=1,m=1,a=1,s=1}""".stripMargin

  lazy val testInput =
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}
      |""".stripMargin

  def input = ParseUtils.inputForDay(19)


}
