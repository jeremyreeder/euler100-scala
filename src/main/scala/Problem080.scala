/* Problem 80: Square Root Digital Expansion
It is well known that if the square root of a natural number is not an integer, then it is irrational. The decimal
expansion of such square roots is infinite without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits
for all the irrational square roots.
*/
import java.math.{BigDecimal, MathContext}

object Problem080 extends App {
	def isRational(n: Int) =
		val root = math.sqrt(n)
		root == root.floor
	
	val plentyOfPrecision = MathContext(102)
	
	def firstHundredDecimalDigitsOfSquareRoot(n: Int) =
		BigDecimal(n).sqrt(plentyOfPrecision).toString.replaceFirst("\\.", "").map(_.asDigit).take(100)
	
	val answer = (1 to 100).filterNot(isRational).flatMap(firstHundredDecimalDigitsOfSquareRoot).sum
	
	println(answer)
}