// https://adventofcode.com/2023/day/7

@main def run = {
  val input =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

  def getCardValue (card: Char) = {
    card match {
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case 'T' => 10
    case _ => card.asDigit
    }
  }

  object HandTypeValues {
  val HighCard = 1
  val OnePair = 2
  val TwoPair = 3
  val ThreeOfAKind = 4
  val FullHouse = 5
  val FourOfAKind = 6
  val FiveOfAKind = 7
}
  
  /* 
  inspirated by:
  https://scalacenter.github.io/scala-advent-of-code/2023/puzzles/day07 
  */
  object HandType {
  def apply(hand: String): Int = {
    val cardGroups : List[Int] = hand.groupBy(identity).values.toList.map(_.length).sorted.reverse

    cardGroups match
      case 5 :: _ => HandTypeValues.FiveOfAKind
      case 4 :: _ => HandTypeValues.FourOfAKind
      case 3 :: 2 :: Nil => HandTypeValues.FullHouse
      case 3 :: _ => HandTypeValues.ThreeOfAKind
      case 2 :: 2 :: _ => HandTypeValues.TwoPair
      case 2 :: _ => HandTypeValues.OnePair
      case _ => HandTypeValues.HighCard
  }
  }

  def compareHands(hand1: String, hand2: String): Boolean = {
  val handType1 = HandType(hand1)
  val handType2 = HandType(hand2)

  (handType1, handType2) match {
    case (h1, h2) if h1 != h2 => h1 < h2
    case _ =>
      val cardValues1 = hand1.map(getCardValue)
      val cardValues2 = hand2.map(getCardValue)
      
      cardValues2.zipAll(cardValues1, 0, 0).collectFirst {
        case (value2, value1) if value1 != value2 => value2 - value1
      }.getOrElse(0) > 0
  }
}

  val inputLines = input.split("\n")

  val handsAndBids = inputLines.map(line => {
  val handAndBid = line.split(" ")
  (handAndBid.head, handAndBid.tail.head.toInt)
  })

  val sortedHandsAndBids = handsAndBids.sortWith((h1, h2) => compareHands(h1._1, h2._1))
  
  val totalWinnings = sortedHandsAndBids.zipWithIndex.map {
    case ((_, bid), index) => bid * (index + 1)
  }.sum

  println(totalWinnings)
}

run
