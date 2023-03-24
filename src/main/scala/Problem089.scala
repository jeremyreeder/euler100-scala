/* Problem 89: Roman Numerals
For a number written in Roman numerals to be considered valid there are basic rules which must be followed. Even though
the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular
number.

For example, it would appear that there are at least six ways of writing the number sixteen:
	IIIIIIIIIIIIIIII
	VIIIIIIIIIII
	VVIIIIII
	XIIIIII
	VVVI
	XVI

However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most
efficient, as it uses the least number of numerals.

The 11K text file, p089_roman.txt, contains one thousand numbers written in valid, but not necessarily minimal, Roman
numerals; see [About... Roman Numerals](https://projecteuler.net/about=roman_numerals) for the definitive rules for this
problem.

Find the number of characters saved by writing each of these in their minimal form.
Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.
*/
import scala.io.Source

object Problem089 extends App {
	
	def compact(romanNumber: String) =
		romanNumber
			.replaceFirst("VIIII", "IX")
			.replaceFirst("IIII", "IV")
			.replaceFirst("XXXX", "XL")
			.replaceFirst("LXL", "XC")
			.replaceFirst("DCCCC", "CM")
			.replaceFirst("CCCC", "CD")
	
	def answer =
		val romanFile = Source fromFile "p089_roman.txt"
		val romanNumbers = romanFile.getLines.toList
		for number <- romanNumbers do
			println(s"$number -> ${compact(number)}")
		val charCountBefore = romanNumbers.map(_.size).sum
		val charCountAfter = romanNumbers.map(compact(_).size).sum
		romanFile.close()
		charCountBefore - charCountAfter
	
	println(answer)
}