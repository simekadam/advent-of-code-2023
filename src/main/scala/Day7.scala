object Day7 {
  def main(args: Array[String]): Unit = {
    val lines = {
      scala.io.Source.fromFile(args.head).getLines.toVector
    }

    val t1 = System.currentTimeMillis()
    val hands = lines.map { line =>
      val handCardAndBids = line.split(" ")
      val handCards = handCardAndBids(0).toCharArray.map((value: Char) => CARDS(value))
      val counts = handCards.foldLeft(Map.empty[Int, Int]) { (map: Map[Int, Int], card: Int) =>
        map + (card -> (map.getOrElse(card, 0) + 1))
      }
      Hand(handCards.toSeq, counts, handCardAndBids(1).toInt)
    }
    println("Part 1: " + solutionPart1(hands) + " 250957639")
    println("Part 2: " + solutionPart2(hands) + " 251515496")
    println("Duration: " + (System.currentTimeMillis() - t1))
  }

  private val CARDS = Map(
    'A' -> 13,
    'K' -> 12,
    'Q' -> 11,
    'J' -> 10,
    'T' -> 9,
    '9' -> 8,
    '8' -> 7,
    '7' -> 6,
    '6' -> 5,
    '5' -> 4,
    '4' -> 3,
    '3' -> 2,
    '2' -> 1
  )


  case class Hand(cards: Seq[Int], counts: Map[Int, Int], bid: Int) {
    def part1Value(): Int = {
      getValue(counts.values.toList.sorted.reverse, cards.size)
    }

    def part2Value(): Int = {
      val jokerCount = counts.getOrElse(10, 0)
      var sorted: List[Int] = List()
      if (jokerCount > 0 && jokerCount < cards.size) {
        sorted = counts.filter { case (key, _) => key != 1 }.values.toList.sorted.reverse
        sorted = sorted.updated(0, sorted.head + jokerCount)
      } else {
        sorted = counts.values.toList.sorted.reverse
      }

      getValue(sorted, cards.size)
    }
  }

  private def getValue(counts: Seq[Int], size: Int): Int = {
    counts.zipWithIndex.foldLeft(0) { (acc: Int, countIndexTuple: (Int, Int)) =>
        val num = if (countIndexTuple._2 < counts.size) {
          countIndexTuple._1
        } else {
          0
        }
        acc + num * Math.pow(10, size - countIndexTuple._2).toInt
      }
    }

  private case class HandOrdering(part2: Boolean) extends Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xValue = if (part2) x.part2Value() else x.part1Value()
      val yValue = if (part2) y.part2Value() else y.part1Value()

      val comparison = xValue - yValue
      if (comparison != 0) {
        comparison
      } else {
        x.cards.zip(y.cards).find { pair: (Int, Int) =>
          pair._1 - pair._2 != 0
        }.map{pair: (Int, Int) => pair._1 - pair._2}.getOrElse(0)
      }
    }
  }


  private def solutionPart1(hands: Seq[Hand]): Long = {
    hands.sorted(HandOrdering(false)).zipWithIndex.map { handAndIndexPair: (Hand, Int) =>
      handAndIndexPair._1.bid * (handAndIndexPair._2 + 1)
    }.sum
  }

  private def solutionPart2(hands: Seq[Hand]): Long = {
    hands.map { hand =>
      val mapped = hand.cards.map {
        case card@(13 | 12 | 11) => card
        case 10 => 1
        case card => card + 1
      }
        Hand(mapped , hand.counts, hand.bid)
    }.sorted(HandOrdering(true)).zipWithIndex.map { handAndIndexPair: (Hand, Int) =>
      handAndIndexPair._1.bid * (handAndIndexPair._2 + 1)
    }.sum
  }
}