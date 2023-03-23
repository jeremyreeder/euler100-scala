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

The 11K text file, 089_roman.txt, contains one thousand numbers written in valid, but not necessarily minimal, Roman
numerals; see [About... Roman Numerals](https://projecteuler.net/about=roman_numerals) for the definitive rules for this
problem.

Find the number of characters saved by writing each of these in their minimal form.
Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.
*/
/* Additive Rules
	1. Numerals must be arranged in descending order of size.
	2. M, C, and X cannot be equalled or exceeded by smaller denominations.
	3. D, L, and V can each only appear once.
*/
/* Subtractive Rules
	4. Only one I, X, and C can be used as the leading numeral in part of a subtractive pair.
	5. I can only be placed before V and X.
	6. X can only be placed before L and C.
	7. C can only be placed before D and M.
*/
import scala.io.Source

object Problem089 extends App {
	
	def compact(romanNumber: String) =
		romanNumber
			.replaceFirst("VIIII", "IX")
			.replaceFirst("IIII", "IV")
			.replaceFirst("XIIII", "XC")
			.replaceFirst("XXXX", "XL")
			.replaceFirst("DCCCC", "CM")
			.replaceFirst("CCCC", "CD")
	
	def answer =
		val romanFile = Source fromFile "089_roman.txt"
		val romanNumbers = romanFile.getLines.toList
		for number <- romanNumbers do
			println(s"$number -> ${compact(number)}")
		val charCountBefore = romanNumbers.map(_.size).sum
		val charCountAfter = romanNumbers.map(compact(_).size).sum
		romanFile.close()
		charCountBefore - charCountAfter
	
	println(answer) // 695, but that's wrong.
}