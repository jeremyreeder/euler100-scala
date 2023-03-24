import scala.io.Source

// Problem 54: Poker Hands
object Problem054 extends App {
	def cardRank(card: String) = card.charAt(0) match {
		case '2' => 1
		case '3' => 2
		case '4' => 4
		case '5' => 8
		case '6' => 16
		case '7' => 32
		case '8' => 64
		case '9' => 128
		case 'T' => 256
		case 'J' => 512
		case 'Q' => 1024
		case 'K' => 2048
		case 'A' => 4096
		case _ => 0
	}
	
	val orderByCardRank = new Ordering[String] {
		override def compare(a: String, b: String) =
			cardRank(a) compare cardRank(b)
	}
	
	def cardSuit(card: String) = card.charAt(1)
	
	def handRank(h: Iterable[String]) = h.toList.sorted(orderByCardRank) match {
		case h if h.map(cardSuit).toSet.size == 1 && h.map(cardRank).sum == 7936 =>
			10 // royal flush
		case h if h.map(cardSuit).toSet.size == 1 && h.map(cardRank).sum.toBinaryString.contains("11111") =>
			9 // straight flush
		case h if h.map(cardRank(_).toBinaryString.toLong).sum.toString.contains("4") =>
			8 // 4 of a kind
		case h if h.map(cardRank(_).toBinaryString.toLong).sum.toString.sorted contains "23" =>
			7 // full house
		case h if h.map(cardSuit).toSet.size == 1 =>
			6 // flush
		case h if h.map(cardRank).sum.toBinaryString.contains("11111") =>
			5 // straight
		case h if h.map(cardRank(_).toBinaryString.toLong).sum.toString.contains("3") =>
			4 // 3 of a kind
		case h if h.map(cardRank(_).toBinaryString.toLong).sum.toString.count(_ == '2') == 2 =>
			3 // 2 pair
		case h if h.map(cardRank(_).toBinaryString.toLong).sum.toString.count(_ == '2') == 1 =>
			2 // 1 pair
		case h =>
			1 // high card
	}
	
	def compareHands(a: Iterable[String], b: Iterable[String]): Int =
		val (handRankA, handRankB) = (handRank(a), handRank(b))
		val handRankComparison = handRankA compare handRankB
		val firstPass =
			(handRankA, handRankComparison) match {
				case (_, comparison) if comparison != 0 =>
					comparison
				case (10, _) =>
					0 // unbreakable tie: royal flush
				case (rank, _) if Seq(8, 7, 4, 3, 2) contains rank => // pair(s), N-of-a-kind, or full house
					val valueOfGroupsA = a.map(cardRank).groupBy(identity).values.maxBy(_.size).head
					val valueOfGroupsB = b.map(cardRank).groupBy(identity).values.maxBy(_.size).head
					valueOfGroupsA compare valueOfGroupsB // tie broken by value of largest group
				case _ => 0
			}
		if firstPass != 0 then firstPass
		else a.map(cardRank).sum compare b.map(cardRank).sum // tie broken by highest card
	
	def outcomes =
		for {
			round <- Source.fromFile("p054_poker.txt").getLines
			cards = round.split(" ")
			(handA, handB) = (cards take 5, cards takeRight 5)
		} yield compareHands(handA, handB)
	
	val answer = outcomes.filter(_ > 0).size
	println(answer)
}