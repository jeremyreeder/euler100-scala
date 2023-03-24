/* Problem 70: Totient Permutation
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers
less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine
and relatively prime to nine, φ(9)=6.

The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
*/
object Problem070 extends App {
	def isPermutation(n: Int, m: Int): Boolean = {
		n.toString.sorted == m.toString.sorted
	}
	
	val Limit = 9_999_999
	val phi = Array.fill(Limit + 1)(-1)
	
	// calculate phi for primes
	(2 to Limit).foreach { n =>
		if phi(n) == -1 then
			// n is prime
			phi(n) = n - 1
			(n * 2 to Limit by n).foreach { m =>
				if phi(m) == -1 then phi(m) = m
				phi(m) = phi(m) / n * (n - 1)
			}
	}
	
	// search for largest n such that phi(n) is a permutation of n
	var minRatio = Double.MaxValue
	var answer = -1
	(2 to Limit).foreach { n =>
		if isPermutation(n, phi(n)) then
			val ratio = n.toDouble / phi(n)
			if ratio < minRatio then
				minRatio = ratio
				answer = n
	}
	
	println(answer)
}