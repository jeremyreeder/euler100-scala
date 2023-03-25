/*Problem 99: Largest Exponential
Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would confirm that
211 = 2048 < 37 = 2187.

However, confirming that 632382518061 > 519432525806 would be much more difficult, as both numbers contain over three
million digits.

Using base_exp.txt, a 22K text file containing one thousand lines with a base/exponent pair on each line, determine
which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given above.
*/
import scala.io.Source
import scala.math.log

object Problem099 extends App {
	def compareIndexForms(a: (Int, Int), b: (Int, Int)) =
		val (baseA, expA) = a
		val (baseB, expB) = b
		val logA = log(baseA) * expA
		val logB = log(baseB) * expB
		logA compare logB
	
	val numbers =
		val file = Source fromFile "p099_base_exp.txt"
		val lines = file.getLines.toList
		file.close()
		lines.map(_.split(',')).map(n => (n(0).toInt, n(1).toInt))
	
	val answer =
		val greatestNumber = numbers.sorted(compareIndexForms).last
		numbers.indexOf(greatestNumber) + 1
	
	println(answer)
}