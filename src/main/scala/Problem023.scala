import scala.collection.mutable

/* Problem 23: Non-abundant Sums
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the
sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum
exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of
two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be
written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even
though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*/
object Problem023 extends App {
	private def properDivisors(n: Int) = 1 to n / 2 filter (n % _ == 0)
	
	def memoize[I, O](f: I => O): I => O =
		new mutable.HashMap[I, O]() {
			override def apply(key: I) = getOrElseUpdate(key, f(key))
		}
	
	private lazy val isAbundant: Int => Boolean = memoize {
		n => properDivisors(n).sum > n
	}
	val limit = 28123
	val abundantPairs =
		for x <- 1 to limit - 1 if isAbundant(x)
		    y <- x to limit - x if isAbundant(y)
		yield (x, y)
	private val numbersWritableAsSumOfTwoAbundantSums = {
		abundantPairs map (_ + _)
	}.distinct
	private val numbersNotWritableThatWay = 1 to limit filterNot (numbersWritableAsSumOfTwoAbundantSums contains _)
	private val answer = numbersNotWritableThatWay.sum
	println(answer)
}