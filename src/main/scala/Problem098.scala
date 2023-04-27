import io.Source
import math.sqrt

object Problem098 extends App {
	
	val anagramWordPairs = {
		val Words =
			val file = Source fromFile "p098_words.txt"
			val lines = file.getLines.toList
			file.close()
			lines.head.split(',').map(_.replaceAll("\"", ""))
		
		def priorAnagramsOf(word: String) =
			Words.takeWhile(_ != word).filter(_.length == word.length).filter(_.sorted == word.sorted)
		
		for word <- Words; anagram <- priorAnagramsOf(word)
			yield (word, anagram)
	}
	
	private def isSquare(n: Int) = { val root = sqrt(n); root == root.floor }
	
	def greaterOfSquarePair(word: String, anagram: String): Option[Int] =
		for key <- (word + "@@@@@@@@").take(10).permutations do
			var (wordValue, anagramValue) = (word, anagram)
			for letter <- word.distinct do
				wordValue = wordValue.replace(letter, (key indexOf letter).toString.head)
				anagramValue = anagramValue.replace(letter, (key indexOf letter).toString.head)
			if !(wordValue.head == '0') && !(anagramValue.head == '0') then
				val (w, a) = (wordValue.toInt, anagramValue.toInt)
				if isSquare(a) && isSquare(w) then return Some(w max a)
		None
	
	val answer = anagramWordPairs.map(greaterOfSquarePair).max.get
	println(answer)
}