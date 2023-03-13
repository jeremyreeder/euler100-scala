/* Problem 26: Reciprocal Cycles
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10
are given:
  1/2	= 	0.5
  1/3	= 	0.(3)
  1/4	= 	0.25
  1/5	= 	0.2
  1/6	= 	0.1(6)
  1/7	= 	0.(142857)
  1/8	= 	0.125
  1/9	= 	0.(1)
  1/10	= 	0.1
  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring
  cycle.
Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
*/
object Problem026 extends App {
	private def cycleLength(q: Int): Int =
		if q > 1 then // Floyd's algorithm
			var length = 1
			var tortoise = 1 % q
			var hare = 10 % q
			while tortoise != hare do
				tortoise = (tortoise * 10) % q
				hare = (hare * 10) % q
				hare = (hare * 10) % q
				length += 1
			length
		else 0
	
	val answer = (2 until 1000).distinct.maxBy(cycleLength(_))
	println(answer)
}