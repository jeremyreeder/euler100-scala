/* Problem 71: Ordered Fractions
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper
fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
	1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the
fraction immediately to the left of 3/7.
*/
import scala.annotation.tailrec

object Problem071 extends App {
	@tailrec
	def hcf(a: Int, b: Int): Int =
		if b == 0 then a else hcf(b, a % b)
		
	val fractions =
		for {
			d <- 2 to 1_000_000
			n = d * 3 / 7
			if (n, d) != (3, 7) && hcf(n, d) == 1
		} yield (n, d)
	
	val byFractionValue: Ordering[(Int, Int)] = Ordering.by(_.toDouble / _)
	
	val answer = fractions.sorted(byFractionValue).last._1
	println(answer)
}