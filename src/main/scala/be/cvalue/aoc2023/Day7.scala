package be.cvalue.aoc2023

import be.cvalue.aoc2023.Day7.*
import be.cvalue.common.ParseUtils
import be.cvalue.common.ParseUtils.*

import scala.math.Ordered.orderingToOrdered

object Day7 extends App {

  type Hand = List[Card]

  enum Card(val face: Char) {
    case A extends Card('A')
    case K extends Card('K')
    case Q extends Card('Q')
    case J extends Card('J')
    case T extends Card('T')
    case Nine extends Card('9')
    case Eight extends Card('8')
    case Seven extends Card('7')
    case Six extends Card('6')
    case Five extends Card('5')
    case Four extends Card('4')
    case Three extends Card('3')
    case Two extends Card('2')

    override def toString: String = face.toString
  }


  object Card {
    val order = values.sortBy(_.ordinal)

    val compOrdinal: Ordering[Card] = (o1: Card, o2: Card) => o1.ordinal.compare(o2.ordinal) * -1

    val compJLowest: Ordering[Card] = (o1: Card, o2: Card) => {
      if (o1 == J && o2 == J) {
        0
      } else if (o1 == J) {
        -1
      } else if (o2 == J) {
        1
      } else {
        o1.ordinal.compare(o2.ordinal) * -1
      }
    }
  }

  sealed trait HandType {
    def rank: Int
  }

  case object FiveOfAKind extends HandType {
    val rank = 7
  }

  case object FourOfAKind extends HandType {
    val rank = 6
  }

  case object FullHouse extends HandType {
    val rank = 5
  }

  case object ThreeOfAKind extends HandType {
    val rank = 4
  }

  case object TwoPair extends HandType {
    val rank = 3
  }

  case object OnePair extends HandType {
    val rank = 2
  }

  case object High extends HandType {
    val rank = 1
  }

  implicit def compGame(implicit cardOrdering: Ordering[Card]): Ordering[Game] = (o1: Game, o2: Game) => {
    val rank = o1.handType.rank.compare(o2.handType.rank)
    if (rank != 0) {
      rank
    } else {
      o1.tupled.compare(o2.tupled)
    }
  }

  object HandType {

    def calc(hand: List[Card], replaceJ: Boolean)(implicit cardOrdering: Ordering[Card]): HandType = {
      val highest = hand.max
      val grouped = if (replaceJ) {
        hand.filter(_ != Card.J).groupBy(identity).view.mapValues(_.size)
      } else {
        hand.groupBy(identity).view.mapValues(_.size)
      }

      grouped.values.toList.sorted.reverse match
        case List(5) => FiveOfAKind
        case 4 :: rest => FourOfAKind
        case 3 :: 2 :: Nil => FullHouse
        case 3 :: rest => ThreeOfAKind
        case 2 :: 2 :: rest => TwoPair
        case 2 :: rest => OnePair
        case 1 :: rest => High
        case Nil => FiveOfAKind
    }
  }

  case class Game(hand: Hand, bid: Int, handType: HandType) {

    val tupled = hand match {
      case c1 :: c2 :: c3 :: c4 :: c5 :: Nil => (c1, c2, c3, c4, c5)
    }

    def promote: Game = {
      val jokerCount = hand.count(_ == Card.J)
      if (jokerCount == 0) {
        this
      } else {
        val promotedHandType = handType match {
          case FiveOfAKind => FiveOfAKind
          case FourOfAKind => FiveOfAKind
          case FullHouse => jokerCount match {
            case 2 => FiveOfAKind
          }
          case ThreeOfAKind => jokerCount match {
            case 2 => FiveOfAKind
            case 1 => FourOfAKind
          }
          case TwoPair => jokerCount match {
            case 1 => FullHouse
          }
          case OnePair => jokerCount match {
            case 3 => FiveOfAKind
            case 2 => FourOfAKind
            case 1 => ThreeOfAKind
          }
          case High => jokerCount match {
            case 4 => FiveOfAKind
            case 3 => FourOfAKind
            case 2 => ThreeOfAKind
            case 1 => OnePair
          }
        }
        this.copy(handType = promotedHandType)
      }
    }

    override def toString: String = {
      s"${hand.mkString} $bid ($handType)"
    }
  }

  def parse(line: String, jIsJoker: Boolean)(implicit cardOrdering: Ordering[Card]): Game = {
    line.split(' ').toList match {
      case cardS :: bid :: Nil =>
        val cards = cardS.trim.map(c => Card.values.find(_.face == c).get).toList
        if (jIsJoker) {
          Game(cards, bid.trim.toInt, HandType.calc(cards, jIsJoker)).promote
        } else {
          Game(cards, bid.trim.toInt, HandType.calc(cards, jIsJoker))
        }
    }
  }

  def part1(input: String): Long = {
    val games = stringToLines(input).map(parse(_, false)(Card.compOrdinal))

    games.sorted(compGame(Card.compOrdinal)).zipWithIndex.map((g, index) => g.bid.toLong * (index + 1)).sum
  }

  def part2(input: String): Long = {
    val games = stringToLines(input).map(parse(_, true)(Card.compJLowest))
    games.sorted(compGame(Card.compJLowest)).zipWithIndex.map((g, index) => g.bid * (index + 1)).sum
  }


  println(part1(input))
  println(part2(input))

  lazy val input2 =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  def input = ParseUtils.inputForDay(7)

}
