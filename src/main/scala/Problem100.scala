/* Problem 100: Arranged Probability
If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken
at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box containing
eighty-five blue discs and thirty-five red discs.

By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, determine the number of blue
discs that the box would contain.
*/
object Problem100 extends App {
	// b/d * (b-1)/(d-1) = 1/2
	// d/b ~= sqrt(2); d/b < sqrt(2); (d-1)/(b-1) > sqrt(2)
	
	extension (n: BigInt) {
		def sqrt =
			val d = BigDecimal(n)
			var a = BigDecimal(1)
			var b = d
			while b >= a do
				val mid = (a + b) / 2
				if mid * mid > d then b = mid - 0.000_001
				else a = mid + 0.000_001
			b
	}
	
	val MinD = BigInt(1_000_000_000_000L)
	
	def solutions =
		for {
			d <- LazyList.range(MinD, MinD + Int.MaxValue)
			b = ((2 * d * d - 2 * d + 1).sqrt + 1) / 2
				if b.isWhole
		} yield (b, d)
	
	println(BigInt(81).sqrt)
	solutions.take(14).foreach(println)
	//println(answer) // 1_168_070_918 is wrong.
	// TODO: Solve this problem.
}