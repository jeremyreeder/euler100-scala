/* Problem 72: Counting Fractions
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
	1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.
How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?
*/
object Problem072 extends App {
	val Limit = 1_000_000
	val phi = Array.fill(Limit + 1)(-1L)
	
	// calculate phi for primes
	(2 to Limit).foreach { n =>
		if phi(n) == -1L then
			// n is prime
			phi(n) = n.toLong - 1
			(n * 2 to Limit by n).foreach { m =>
				if phi(m) == -1L then phi(m) = m
				phi(m) = phi(m) / n * (n - 1)
			}
	}
	
	def properFractionCount(maxD: Int) = (2 to maxD).map(phi).sum
	
	val answer = properFractionCount(Limit)
	println(answer)
}